# include <stdio.h>
# include <pwd.h>
# include "dlvrmail.h"

/*
**  SAVEMAIL -- Save mail on error
**
**	If the MailBack flag is set, mail it back to the originator
**	together with an error message; otherwise, just put it in
**	dead.letter in the user's home directory (if he exists on
**	this machine).
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Saves the letter, by writing or mailing it back to the
**		sender, or by putting it in dead.letter in her home
**		directory.
**
**		WARNING: the user id is reset to the original user.
**
**	Requires:
**		fopen (sys)
**		bmove
**		parse
**		deliver
**		strcpy (sys)
**		strcat (sys)
**		fclose (sys)
**		fgets (sys)
**		fputs (sys)
**		setpwent (sys)
**		getuid (sys)
**		setuid (sys)
**		getgid (sys)
**		setgid (sys)
**		getpwnam (sys)
**		fprintf (sys)
**		ttypath
**		freopen (sys)
**		printf (sys)
**		syserr
**		rewind (sys)
**		time (sys)
**		ferror (sys)
**
**	Called By:
**		finis
**
**	History:
**		12/30/79 -- written.
*/

# define MY_NAME	"~MAILER~DAEMON~"

savemail()
{
	register struct passwd *pw;
	register FILE *xfile;
	char buf[MAXLINE+1];
	extern errhdr();
	auto addrq to_addr;
	extern struct passwd *getpwnam();
	register char *p;
	register int i;
	auto long tim;
	extern int errno;
	extern char *ttypath();
	extern char *ctime();
	extern addrq *parse();
	static int exclusive;

	if (exclusive++)
		return;

	/*
	**  In the unhappy event we don't know who to return the mail
	**  to, make someone up.
	*/

	if (From.q_paddr == NULL)
	{
		if (parse("root", &From, 0) == NULL)
		{
			syserr("Cannot parse root!");
			ExitStat = EX_SOFTWARE;
			finis();
		}
	}

	/*
	**  If called from Eric Schmidt's network, do special mailback.
	**	Fundamentally, this is the mailback case except that
	**	it returns an OK exit status (assuming the return
	**	worked).
	*/

	if (BerkNet)
	{
		ExitStat = EX_OK;
		MailBack++;
	}

	/*
	**  If writing back, do it.
	**	If the user is still logged in on the same terminal,
	**	then write the error messages back to hir (sic).
	**	If not, set the MailBack flag so that it will get
	**	mailed back instead.
	*/

	if (WriteBack)
	{
		p = ttypath();
		if (p == NULL || freopen(p, "w", stdout) == NULL)
		{
			MailBack++;
			errno = 0;
		}
		else
		{
			xfile = fopen(Transcript, "r");
			if (xfile == NULL)
				syserr("Cannot open %s", Transcript);
			printf("\r\nMessage from %s\r\n", MY_NAME);
			printf("Errors occurred while sending mail, transcript follows:\r\n");
			while (fgets(buf, sizeof buf, xfile) && !ferror(stdout))
				fputs(buf, stdout);
			if (ferror(stdout))
				syserr("savemail: stdout: write err");
			fclose(xfile);
		}
	}

	/*
	**  If mailing back, do it.
	**	Throw away all further output.  Don't do aliases, since
	**	this could cause loops, e.g., if joe mails to x:joe,
	**	and for some reason the network for x: is down, then
	**	the response gets sent to x:joe, which gives a
	**	response, etc.  Also force the mail to be delivered
	**	even if a version of it has already been sent to the
	**	sender.
	*/

	if (MailBack || From.q_mailer != &Mailer[0])
	{
		freopen("/dev/null", "w", stdout);
		NoAlias++;
		ForceMail++;

		/* fake up an address header for the from person */
		bmove((char *) &From, (char *) &to_addr, sizeof to_addr);
		if (parse(MY_NAME, &From, -1) == NULL)
		{
			syserr("Can't parse myself!");
			ExitStat = EX_SOFTWARE;
			finis();
		}
		i = deliver(&to_addr, errhdr);
		bmove((char *) &to_addr, (char *) &From, sizeof From);
		if (i != 0)
			syserr("Can't return mail to %s", p);
		else
			return;
	}

	/*
	**  Save the message in dead.letter.
	**	If we weren't mailing back, and the user is local, we
	**	should save the message in dead.letter so that the
	**	poor person doesn't have to type it over again --
	**	and we all know what poor typists programmers are.
	*/

	setuid(getuid());
	setgid(getgid());
	setpwent();
	if (From.q_mailer == &Mailer[0] && (pw = getpwnam(From.q_user)) != NULL)
	{
		/* user has a home directory */
		p = pw->pw_dir;
	}
	else
	{
		syserr("Can't return mail to %s (pw=%u)", From.q_paddr, pw);
# ifdef DEBUG
		p = "/usr/tmp";
# else
		p = NULL;
# endif
	}
	if (p != NULL)
	{
		/* we have a home directory; open dead.letter */
		strcpy(buf, p);
		strcat(buf, "/dead.letter");
		xfile = fopen(buf, "a");
		if (xfile == NULL)
			printf("Cannot save mail, sorry\n");
		else
		{
			rewind(stdin);
			errno = 0;
			time(&tim);
			fprintf(xfile, "----- Mail saved at %s", ctime(&tim));
			while (fgets(buf, sizeof buf, stdin) && !ferror(xfile))
				fputs(buf, xfile);
			fputs("\n", xfile);
			if (ferror(xfile))
				syserr("savemail: dead.letter: write err");
			fclose(xfile);
			printf("Letter saved in dead.letter\n");
		}
	}
	else

	/* add terminator to writeback message */
	if (WriteBack)
		printf("-----\r\n");
}
/*
**  ERRHDR -- Output the header for error mail.
**
**	This is the edit filter to error mailbacks.
**
**	Algorithm:
**		Output fixed header.
**		Output the transcript part.
**		Output the original message.
**
**	Parameters:
**		xfile -- the transcript file.
**		fp -- the output file.
**
**	Returns:
**		none
**
**	Side Effects:
**		input from xfile
**		output to fp
**
**	Requires:
**		read (sys)
**		write (sys)
**		open (sys)
**		close (sys)
**		syserr
**		rewind (sys)
**		fflush (sys)
**		fprintf (sys)
**		fileno (sys)
**
**	Called By:
**		deliver
**
**	History:
**		12/28/79 -- written.
*/


errhdr(fp)
	register FILE *fp;
{
	char copybuf[512];
	register int i;
	register int xfile;
	extern int errno;

	if ((xfile = open(Transcript, 0)) < 0)
		syserr("Cannot open %s", Transcript);
	fflush(stdout);
	errno = 0;
	fprintf(fp, "To: %s\n", To);
	fprintf(fp, "Subject: Unable to deliver mail\n");
	fprintf(fp, "\n   ----- Transcript of session follows -----\n");
	fflush(fp);
	while ((i = read(xfile, copybuf, sizeof copybuf)) > 0)
		write(fileno(fp), copybuf, i);
	fprintf(fp, "\n   ----- Unsent message follows -----\n");
	fflush(fp);
	rewind(stdin);
	while ((i = read(fileno(stdin), copybuf, sizeof copybuf)) > 0)
		write(fileno(fp), copybuf, i);
	close(xfile);
	if (errno != 0)
		syserr("errhdr: I/O error");
}
