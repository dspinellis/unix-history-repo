# include	<stdio.h>
# include	<ctype.h>
# include	<signal.h>
# include	<sysexits.h>
# include	"useful.h"

static char SccsId[] = "@(#)arpa.c	2.2	1/11/81";
char Version[] = "@(#)Arpa-mailer version 2.2 of 1/11/81";


/*
**  ARPA MAILER -- Queue ARPANET mail for eventual delivery
**
**	The standard input is stuck away in the outgoing arpanet
**	mail queue for delivery by the true arpanet mailer.
**
**	Usage:
**		/usr/lib/mailers/arpa from host user
**
**	Positional Parameters:
**		from -- the person sending the mail.
**		host -- the host to send the mail to.
**		user -- the user to send the mail to.
**
**	Flags:
**		-T -- debug flag.
**
**	Files:
**		/usr/spool/netmail/* -- the queue file.
**
**	Return Codes:
**		0 -- all messages successfully mailed.
**		2 -- user or host unknown.
**		3 -- service unavailable, probably temporary
**			file system condition.
**		4 -- syntax error in address.
**
**	Compilation Flags:
**		SPOOLDIR -- the spool directory
**
**	Compilation Instructions:
**		cc -n -O -s arpa-mailer.c -o arpa-mailer -lX
**		chmod 755 arpa-mailer
**		mv arpa-mailer /usr/lib/mailers/arpa
**
**	Author:
**		Eric Allman, UCB/INGRES (eric@berkeley)
*/

# define SPOOLDIR	"/usr/spool/netmail"




char	*From;			/* person sending this mail */
char	*To;			/* current "To:" person */
int	State;			/* the current state (for exit codes) */
# ifdef DEBUG
bool	Tflag;			/* -T given */
# endif DEBUG
char	FromHost[200];		/* string to prepend to addresses */
/*
**  MAIN -- Main program for arpa mailer
**
**	Processes arguments, and calls sendmail successively on
**	the To: list.
**
**	Algorithm:
**		Scan for debug flag.
**		Catch interrupt signals.
**		Collect input file name and from person.
**		If more than one person in the to list, and
**			if the input file is not a real file,
**			collect input into a temp file.
**		For each person in the to list
**			Send to that person.
**
**	Parameters:
**		argc
**		argv -- as usual
**
**	Returns:
**		via exit
**
**	Side Effects:
**		Mail gets sent.
**
**	Called By:
**		/etc/delivermail
**
**	Author:
**		Eric Allman UCB/INGRES.
*/

main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	register char *p;
	register int ifd;
	char buf[512];
	extern int finis();
	extern char *locv();
	register char *q;
	char *lastmark;

	State = 3;
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, finis);
	
	/* process flags */
	argv[argc] = 0;
# ifdef DEBUG
	if (strcmp(argv[1], "-T") == 0)
	{
		Tflag++;
		argv++;
		argc--;
		printf("%s\n", Version);
	}
# endif DEBUG

	if (argc != 4)
	{
		rexit (EX_SOFTWARE);
	}
	
	/* decode parameters */
	From = argv[1];
	lastmark = &FromHost[-1];
	for (p = From, q = FromHost; (*q = *p) != '\0'; p++, q++)
	{
		if (*p == ':')
			*q = *p = '.';
		if (*q == '.' || *q == '!' || *q == '@')
			lastmark = q;
	}
	lastmark[1] = '\0';

	/* start sending mail */
	State = sendmail(argv[2], argv[3]);

	/* all done, clean up */
	finis();
}
/*
**  FINIS -- Finish up, remove temp files, etc.
**
**	This does basic cleanup on interrupt, error, or
**	normal termination.  It uses "State" to tell which
**	is happening.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Exit(State).
**
**	Called By:
**		interrupt signal.
**		main
*/

finis()
{
	rexit(State);
}

/*
** REXIT -- exit, reporting error code if -T given
**
**	Parameters:
**		e -- error code to exit with; see sysexits.h
**
**	Returns:
**		none
**
**	Side Effects:
**		Exit(e).
**
**	Called By:
**		main
**		finis
**		sendmail
*/
rexit(e)
{

# ifdef DEBUG
	if (Tflag)
		fprintf(stderr, "arpa-mail: return code %d\n", e);
# endif
	exit(e);
}
/*
**  SENDMAIL -- Queue up mail for the arpanet mailer.
**
**	The mail is inserted with proper headers into the
**	arpanet queue directory.
**
**	Algorithm:
**		decode "to" address
**			if error, exit.
**		create a spool file name.
**		output the header information to spool file,
**		  separate names in To:, CC: fields with commas.
**		copy the mail to the spool file.
**
**	Parameters:
**		host -- the host to send to.
**		user -- the user to send to.
**
**	Returns:
**		none
**
**	Side Effects:
**		the mail is copied into a file in the network
**			queue directory (/usr/spool/netmail).
**
**	Called By:
**		main
*/

sendmail(host, user)
	char *host;
	char *user;
{
	char spoolfile[50];	/* gets the spool file name */
	register int i;
	register char *p;
	static int callnum;	/* for the final letter on spoolfile */
	char buf[512];
	register FILE *sfp;	/* spool file */
	register int c;
	extern char *matchhdr();

	/* verify that the host exists */
#ifndef TESTING
	strcpy(buf, "/dev/net/");
	strcat(buf, host);
	if (host[0] == '\0' || access(buf, 0) < 0)
		return (EX_NOHOST);
#endif TESTING

	/*
	**  Create spool file name.
	**	Format is "username000nnX", where username is
	**	padded on the right with zeros and nn (the process
	**	id) is padded on the left with zeros; X is a unique
	**	sequence character.
	*/

# ifdef DEBUG
	if (Tflag)
		strcpy(spoolfile, "test.out");
# endif DEBUG
	else
		sprintf(spoolfile, "%s/arpamail%05d%c", SPOOLDIR, getpid(), 'a' + callnum++);

	/* create spool file */
	sfp = fopen(spoolfile, "w");
	if (sfp == NULL)
	{
	spoolerr:
		return (EX_OSERR);
	}
# ifdef DEBUG
	if (!Tflag)
# endif DEBUG
		chmod(spoolfile, 0400);

	/*
	** Output mailer control lines.
	**	These lines are as follows:
	**		/dev/net/<hostname> {target host}
	**		user-name {at target host}
	**		/mnt/eric {pathname of sender; not used}
	**		eric {name of user who is sending}
	*/

	fputs(buf, sfp);
	fputs("\n", sfp);
	fputs(user, sfp);
	fputs("\n\n", sfp);
	fputs(From, sfp);
	fputs("\n", sfp);

	/*
	**  Output the mail
	**	Check the first line for the date.  If not found,
	**	assume the message is not in arpanet standard format
	**	and output a "Date:" and "From:" header.
	*/

	if (fgets(buf, sizeof buf, stdin) == NULL)
	{
		/* no message */
		unlink(spoolfile);
		return (EX_OK);
	}
	if (strncmp("From ", buf, 5) == 0)
	{
		/* strip Unix "From" line */
		/* should save the date here */
		fgets(buf, sizeof buf, stdin);
	}
	if (matchhdr(buf, "date") == NULL)
		putdate(sfp);
	if (!ishdr(buf))
	{
		putc('\n', sfp);
		goto hdrdone;
	}

	/*
	** At this point, we have a message with REAL headers.
	** We look at each head line and insert commas if it
	** is a To: or Cc: field.
	*/

	do
	{
		if (!ishdr(buf))
			break;
		if (!matchhdr(buf, "to") && !matchhdr(buf, "cc"))
		{
			fputs(buf, sfp);
			continue;
		}
		/* gotcha! */
		rewrite(buf, 1, sfp);
		while (isspace(c = peekc(stdin)) && c != '\n')
		{
			fgets(buf, BUFSIZ, stdin);
			rewrite(buf, 0, sfp);
		}
	} while (fgets(buf, BUFSIZ, stdin) != NULL);

hdrdone:
	/* output the rest of the header & the body of the letter */
	do
	{
		fputs(buf, sfp);
		if (ferror(sfp))
			goto spoolerr;
	} while (fgets(buf, sizeof buf, stdin) != NULL);

	/* all done! */
	fclose(sfp);
	return (EX_OK);
}
/*
**  REWRITE -- Output header line with needed commas.
**
**	Parameters:
**		buf -- header line
**		first -- true if this is not a continuation
**
**	Returns:
**		none
**
**	Side effects:
**		The contents of buf is copied onto the spool file with
**		with the right commas interlaced
**
**	Called by:
**		sendmail
*/

rewrite(buf, first, spf)
	char buf[];
	register FILE *spf;
{
	register char *cp;
	register int c;
	char word[BUFSIZ], word2[BUFSIZ];
	char *gword();
	static char wsep[] = ", ";

	cp = buf;
	if (first)
	{
		while (*cp != ':' && *cp)
			putc(*cp++, spf);
		if (*cp == ':')
		{
			fputs(": ", spf);
			cp++;
		}
	}
	else
		while (*cp && isspace(*cp))
			putc(*cp++, spf);
	cp = gword(word, cp);
	if (strlen(word) == 0)
	{
		putc('\n', spf);
		goto test;
	}
	for (;;)
	{
		cp = gword(word2, cp);
		if (strlen(word2) == 0)
		{
			putaddr(word, spf);
			break;
		}
		if (strcmp(word2, "%") == 0)
			word2[0] = '@';
		if (strcmp(word2, "@") && strcmp(word2, "at"))
		{
			putaddr(word, spf);
			fputs(wsep, spf);
			strcpy(word, word2);
			continue;
		}
		fputs(word, spf);
		if (word2[0] == '@')
			putc('@', spf);
		else
			fputs(" at ", spf);
		cp = gword(word, cp);
		fputs(word, spf);
		cp = gword(word, cp);
		if (strlen(word))
			fputs(wsep, spf);
	}

test:
	c = peekc(stdin);
	if (isspace(c) && c != '\n')
		fputs(",\n", spf);
	else
		putc('\n', spf);
}
/*
**  PUTADDR -- output address onto file
**
**	Putaddr prepends the network header onto the address
**	unless one already exists.
**
**	Parameters:
**		name -- the name to output.
**		fp -- the file to put it on.
**
**	Returns:
**		none.
**
**	Side Effects:
**		name is put onto fp.
*/

putaddr(name, fp)
	char *name;
	FILE *fp;
{
	register char *p;

	if (strlen(name) == 0)
		return;
	for (p = name; *p != '\0' && *p != ':' && *p != '.' && *p != '@' &&
	     *p != '!' && *p != '^'; p++)
		continue;
	if (*p == ':')
		*p = '.';
	else if (*p == '\0')
		fputs(FromHost, fp);
	fputs(name, fp);
	if (*p != '@')
		fputs("@Berkeley", fp);
}
/*
**  PEEKC -- peek at next character in input file
**
**	Parameters:
**		fp -- stdio file buffer
**
**	Returns:
**		the next character in the input or EOF
**
**	Side effects:
**		None.
**
**	Called by:
**		sendmail
**		rewrite
*/
peekc(fp)
	register FILE *fp;
{
	register int c;

	c = getc(fp);
	ungetc(c, fp);
	return(c);
}

/*
**  GWORD -- get the next liberal word from a string
**
**	Parameters:
**		buf -- place to put scanned word
**		p -- place to start looking for word
**
**	Returns:
**		updated value of p or 0 if no more left after this
**
**	Side effects:
**		buf gets the liberal word scanned.
**		buf will be length 0 if there is no more input,
**		or if p was passed as 0
**
**	Called by:
**		rewrite
*/
char *
gword(buf, p)
	char buf[];
	register char *p;
{
	register char *sp, *dp;

	strcpy(buf, "");
	if (p == 0)
		return(0);
	sp = p;
	while (*sp && (isspace(*sp) || *sp == ','))
		sp++;
	dp = buf;
	if (*sp != '%' && *sp != '@')
	{
		while (*sp && !isspace(*sp) && *sp != ',' && *sp != '%' && *sp != '@')
			*dp++ = *sp++;
	}
	else
		*dp++ = *sp++;
	*dp = 0;
	if (*sp == 0)
		return(0);
	return(sp);
}
/*
**  ISHDR -- see if the passed line is a ARPA style header line
**
**	Parameters:
**		buf -- header line
**
**	Returns:
**		non-zero if the line is a header line, else zero
**
**	Side effects:
**		none
**
**	Called by:
**		sendmail
*/
ishdr(buf)
	char buf[];
{
	register char *p;

	p = buf;
	if (isspace(*p))
		p = 0;
	else
	{
		while (*p != ':' && !isspace(*p))
			p++;
		while (isspace(*p))
			p++;
		if (*p != ':')
			p = 0;
	}
	return(p != 0);
}
/*
**  PUTDATE -- Put the date & from field into the message.
**
**	Parameters:
**		fp -- file to put them onto.
**
**	Returns:
**		none
**
**	Side Effects:
**		output onto fp.
**
**	Called By:
**		sendmail
*/

putdate(fp)
	register FILE *fp;
{
	register char *p;

	fputs("Date: ", fp);
	fputs(arpadate(), fp);

	/* output from field */
	fputs("\nFrom: ", fp);
	fputs(From, fp);
	fputs(" at Berkeley\n", fp);
}
