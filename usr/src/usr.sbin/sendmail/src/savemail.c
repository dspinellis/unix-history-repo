# include <pwd.h>
# include "sendmail.h"

SCCSID(@(#)savemail.c	4.2		%G%);

/*
**  SAVEMAIL -- Save mail on error
**
**	If mailing back errors, mail it back to the originator
**	together with an error message; otherwise, just put it in
**	dead.letter in the user's home directory (if he exists on
**	this machine).
**
**	Parameters:
**		e -- the envelope containing the message in error.
**
**	Returns:
**		none
**
**	Side Effects:
**		Saves the letter, by writing or mailing it back to the
**		sender, or by putting it in dead.letter in her home
**		directory.
*/

savemail(e)
	register ENVELOPE *e;
{
	register struct passwd *pw;
	register FILE *xfile;
	char buf[MAXLINE+1];
	extern struct passwd *getpwnam();
	register char *p;
	extern char *ttypath();
	typedef int (*fnptr)();

# ifdef DEBUG
	if (tTd(6, 1))
		printf("\nsavemail\n");
# endif DEBUG

	if (exclusive++ || CurEnv->e_class <= PRI_JUNK)
		return;
	ForceMail = TRUE;
	e->e_flags &= ~EF_FATALERRS;

	/*
	**  In the unhappy event we don't know who to return the mail
	**  to, make someone up.
	*/

	if (e->e_from.q_paddr == NULL)
	{
		if (parseaddr("root", &e->e_from, 0, '\0') == NULL)
		{
			syserr("Cannot parse root!");
			ExitStat = EX_SOFTWARE;
			finis();
		}
	}
	e->e_to = NULL;

	/*
	**  If called from Eric Schmidt's network, do special mailback.
	**	Fundamentally, this is the mailback case except that
	**	it returns an OK exit status (assuming the return
	**	worked).
	**  Also, if the from address is not local, mail it back.
	*/

	if (ErrorMode == EM_BERKNET)
	{
		ExitStat = EX_OK;
		ErrorMode = EM_MAIL;
	}
	if (!bitnset(M_LOCAL, e->e_from.q_mailer->m_flags))
		ErrorMode = EM_MAIL;

	/*
	**  If writing back, do it.
	**	If the user is still logged in on the same terminal,
	**	then write the error messages back to hir (sic).
	**	If not, mail back instead.
	*/

	if (ErrorMode == EM_WRITE)
	{
		p = ttypath();
		if (p == NULL || freopen(p, "w", stdout) == NULL)
		{
			ErrorMode = EM_MAIL;
			errno = 0;
		}
		else
		{
			expand("$n", buf, &buf[sizeof buf - 1], e);
			printf("\r\nMessage from %s...\r\n", buf);
			printf("Errors occurred while sending mail.\r\n");
			if (e->e_xfp != NULL)
			{
				(void) fflush(e->e_xfp);
				xfile = fopen(queuename(e, 'x'), "r");
			}
			else
				xfile = NULL;
			if (xfile == NULL)
			{
				printf("Transcript of session is unavailable.\r\n");
			}
			else
			{
				printf("Transcript follows:\r\n");
				while (fgets(buf, sizeof buf, xfile) != NULL &&
				       !ferror(stdout))
					fputs(buf, stdout);
				(void) fclose(xfile);
			}
			if (ferror(stdout))
				(void) syserr("savemail: stdout: write err");
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

	if (ErrorMode == EM_MAIL)
	{
			return;
	}

	/*
	**  Save the message in dead.letter.
	**	If we weren't mailing back, and the user is local, we
	**	should save the message in dead.letter so that the
	**	poor person doesn't have to type it over again --
	**	and we all know what poor typists programmers are.
	*/

	p = NULL;
	if (e->e_from.q_mailer == LocalMailer)
	{
		if (e->e_from.q_home != NULL)
			p = e->e_from.q_home;
		else if ((pw = getpwnam(e->e_from.q_user)) != NULL)
			p = pw->pw_dir;
	}
	if (p == NULL)
	{
		syserr("Can't return mail to %s", e->e_from.q_paddr);
# ifdef DEBUG
		p = "/usr/tmp";
# endif
	}
	if (p != NULL && e->e_dfp != NULL)
	{
		auto ADDRESS *q;
		bool oldverb = Verbose;

		/* we have a home directory; open dead.letter */
		define('z', p, e);
		expand("$z/dead.letter", buf, &buf[sizeof buf - 1], e);
		Verbose = TRUE;
		message(Arpa_Info, "Saving message in %s", buf);
		Verbose = oldverb;
		e->e_to = buf;
		q = NULL;
		sendtolist(buf, (ADDRESS *) NULL, &q);
		(void) deliver(q, (fnptr) NULL);
	}

	/* add terminator to writeback message */
	if (ErrorMode == EM_WRITE)
		printf("-----\r\n");
}
/*
**  RETURNTOSENDER -- return a message to the sender with an error.
**
**	Parameters:
**		msg -- the explanatory message.
**		returnto -- the queue of people to send the message to.
**		sendbody -- if TRUE, also send back the body of the
**			message; otherwise just send the header.
**
**	Returns:
**		zero -- if everything went ok.
**		else -- some error.
**
**	Side Effects:
**		Returns the current message to the sender via
**		mail.
*/

static bool	SendBody;

#define MAXRETURNS	6	/* max depth of returning messages */

returntosender(msg, sendbody)
	char *msg;
	bool sendbody;
{
	char buf[MAXNAME];
	extern errhdr();

# ifdef DEBUG
	if (tTd(6, 1))
	{
		printf("Return To Sender: msg=\"%s\", depth=%d, CurEnv=%x,\n",
		       msg, returndepth, CurEnv);
		printf("\treturnto=");
		printaddr(returnto, TRUE);
	}
# endif DEBUG

	if (++returndepth >= MAXRETURNS)
	{
		if (returndepth != MAXRETURNS)
			syserr("returntosender: infinite recursion on %s", returnto->q_paddr);
		/* don't "unrecurse" and fake a clean exit */
		/* returndepth--; */
		return (0);
	}

	ErrorMessage = msg;
	SendBody = sendbody;
	define('g', "$f", CurEnv);

	/* fake up an address header for the from person */
	bmove((char *) &CurEnv->e_from, (char *) &to_addr, sizeof to_addr);
	(void) expand("$n", buf, &buf[sizeof buf - 1]);
	if (parse(buf, &CurEnv->e_from, -1) == NULL)
	{
		syserr("Can't parse myself!");
		ExitStat = EX_SOFTWARE;
		returndepth--;
		return (-1);
	}

	/* if CurEnv->e_from was queued up, put in on CurEnv->e_sendqueue */
	if (bitset(QQUEUEUP, CurEnv->e_from.q_flags))
	{
		CurEnv->e_from.q_next = CurEnv->e_sendqueue;
		CurEnv->e_sendqueue = &CurEnv->e_from;
	}

	/* should check for delivery errors here */
	return (0);
}
/*
**  ERRHDR -- Output the header for error mail.
**	Parameters:
**		fp -- the output file.
**		xdot -- if set, use smtp hidden dot algorithm.
**
**	Returns:
**		none
**
**	Side Effects:
**		Outputs the current message with an appropriate
**		error header.
*/

errhdr(fp, m, xdot)
	register FILE *fp;
	register struct mailer *m;
	register ENVELOPE *e;
{
	char buf[MAXLINE];
	register FILE *xfile;
	extern char *macvalue();
	char *oldfmac;
	char *oldgmac;

	/*
	**  Output transcript of errors
	*/

	oldfmac = macvalue('f');
	define('f', "$n");
	oldgmac = macvalue('g');
	define('g', m->m_from);

	(void) fflush(stdout);
	(void) fflush(Xscript);
	p = queuename(e->e_parent, 'x');
	if ((xfile = fopen(p, "r")) == NULL)
	{
		syserr("Cannot open %s", p);
		fprintf(fp, "  ----- Transcript of session is unavailable -----\n");
	}
	else
	{
		fprintf(fp, "   ----- Transcript of session follows -----\n");
		if (e->e_xfp != NULL)
			(void) fflush(e->e_xfp);
		while (fgets(buf, sizeof buf, xfile) != NULL)
			putline(buf, fp, m);
		(void) fclose(xfile);
	}
	errno = 0;

	/*
	**  Output "From" line unless supressed
	*/

	if (!bitset(M_NHDR, m->m_flags))
	{
		(void) expand("$l", buf, &buf[sizeof buf - 1]);
		fprintf(fp, "%s\n", buf);
	}

	/*
	**  Output header of error message.
	*/

	if (bitset(M_NEEDDATE, m->m_flags))
	{
		(void) expand("$b", buf, &buf[sizeof buf - 1]);
		fprintf(fp, "Date: %s\n", buf);
	}
	if (bitset(M_NEEDFROM, m->m_flags))
	{
		(void) expand("$g", buf, &buf[sizeof buf - 1]);
		fprintf(fp, "From: %s (Mail Delivery Subsystem)\n", buf);
	}
	fprintf(fp, "To: %s\n", CurEnv->e_to);
	fprintf(fp, "Subject: %s\n", ErrorMessage);

	/*
	**  End of error message header
	*/

	define('f', oldfmac);
	define('g', oldgmac);

	/*
	**  Output text of original message
	*/

	if (NoReturn)
		fprintf(fp, "\n   ----- Return message suppressed -----\n\n");
	else if (e->e_parent->e_dfp != NULL)
	{
		if (SendBody)
		{
			putline("\n", fp, m);
			putline("   ----- Unsent message follows -----\n", fp, m);
			(void) fflush(fp);
			putmessage(fp, Mailer[1], xdot);
		}
		else
		{
			putline("\n", fp, m);
			putline("  ----- Message header follows -----\n", fp, m);
			(void) fflush(fp);
			putheader(fp, m, e->e_parent);
		}
	}
	else
	{
		putline("\n", fp, m);
		putline("  ----- No message was collected -----\n", fp, m);
		putline("\n", fp, m);
	}

	/*
	**  Cleanup and exit
	*/

	if (errno != 0)
		syserr("errhdr: I/O error");
}
