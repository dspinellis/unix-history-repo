/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)savemail.c	8.28 (Berkeley) %G%";
#endif /* not lint */

# include "sendmail.h"
# include <pwd.h>

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

/* defines for state machine */
# define ESM_REPORT	0	/* report to sender's terminal */
# define ESM_MAIL	1	/* mail back to sender */
# define ESM_QUIET	2	/* messages have already been returned */
# define ESM_DEADLETTER	3	/* save in ~/dead.letter */
# define ESM_POSTMASTER	4	/* return to postmaster */
# define ESM_USRTMP	5	/* save in /usr/tmp/dead.letter */
# define ESM_PANIC	6	/* leave the locked queue/transcript files */
# define ESM_DONE	7	/* the message is successfully delivered */

# ifndef _PATH_VARTMP
#  define _PATH_VARTMP	"/usr/tmp/"
# endif


savemail(e)
	register ENVELOPE *e;
{
	register struct passwd *pw;
	register FILE *fp;
	int state;
	auto ADDRESS *q = NULL;
	register char *p;
	MCI mcibuf;
	char buf[MAXLINE+1];
	extern struct passwd *getpwnam();
	extern char *ttypath();
	typedef int (*fnptr)();
	extern bool writable();

	if (tTd(6, 1))
	{
		printf("\nsavemail, errormode = %c, id = %s, ExitStat = %d\n  e_from=",
			e->e_errormode, e->e_id == NULL ? "NONE" : e->e_id,
			ExitStat);
		printaddr(&e->e_from, FALSE);
	}

	if (e->e_id == NULL)
	{
		/* can't return a message with no id */
		return;
	}

	if (exclusive++ || CurEnv->e_class <= PRI_JUNK)
	/*
	**  In the unhappy event we don't know who to return the mail
	**  to, make someone up.
	*/

	if (e->e_from.q_paddr == NULL)
	{
		e->e_sender = "Postmaster";
		if (parseaddr(e->e_sender, &e->e_from,
			      RF_COPYPARSE|RF_SENDERADDR, '\0', NULL, e) == NULL)
		{
			syserr("553 Cannot parse Postmaster!");
			ExitStat = EX_SOFTWARE;
			finis();
		}
	}
	e->e_to = NULL;

	/*
	**  Basic state machine.
	**
	**	This machine runs through the following states:
	**
	**	ESM_QUIET	Errors have already been printed iff the
	**			sender is local.
	**	ESM_REPORT	Report directly to the sender's terminal.
	**	ESM_MAIL	Mail response to the sender.
	**	ESM_DEADLETTER	Save response in ~/dead.letter.
	**	ESM_POSTMASTER	Mail response to the postmaster.
	**	ESM_PANIC	Save response anywhere possible.
	*/

	/* determine starting state */
	switch (e->e_errormode)
	{
	  case EM_WRITE:
		state = ESM_REPORT;
		break;

	  case EM_BERKNET:
		/* mail back, but return o.k. exit status */
		ExitStat = EX_OK;

		/* fall through.... */

	  case EM_MAIL:
		state = ESM_MAIL;
		break;

	  case EM_PRINT:
	  case '\0':
		state = ESM_QUIET;
		break;

	  case EM_QUIET:
		/* no need to return anything at all */
		return;

	  default:
		syserr("554 savemail: bogus errormode x%x\n", e->e_errormode);
		state = ESM_MAIL;
		break;
	}

	/* if this is already an error response, send to postmaster */
	if (bitset(EF_RESPONSE, e->e_flags))
	{
		if (e->e_parent != NULL &&
		    bitset(EF_RESPONSE, e->e_parent->e_flags))
		{
			/* got an error sending a response -- can it */
			return;
		}
		state = ESM_POSTMASTER;
	}

	while (state != ESM_DONE)
	{
		if (tTd(6, 5))
			printf("  state %d\n", state);

		switch (state)
		{
		  case ESM_QUIET:
			if (e->e_from.q_mailer == LocalMailer)
				state = ESM_DEADLETTER;
			else
				state = ESM_MAIL;
			break;

		  case ESM_REPORT:

			/*
			**  If the user is still logged in on the same terminal,
			**  then write the error messages back to hir (sic).
			*/

			p = ttypath();
			if (p == NULL || freopen(p, "w", stdout) == NULL)
			{
				state = ESM_MAIL;
				break;
			}

			expand("\201n", buf, &buf[sizeof buf - 1], e);
			printf("\r\nMessage from %s...\r\n", buf);
			printf("Errors occurred while sending mail.\r\n");
			if (e->e_xfp != NULL)
			{
				(void) fflush(e->e_xfp);
				fp = fopen(queuename(e, 'x'), "r");
			}
			else
				fp = NULL;
			if (fp == NULL)
			{
				printf("Transcript of session is unavailable.\r\n");
			}
			else
			{
				printf("Transcript follows:\r\n");
				while (fgets(buf, sizeof buf, fp) != NULL &&
				       !ferror(stdout))
					fputs(buf, stdout);
				(void) xfclose(fp, "savemail transcript", e->e_id);
			}
			printf("Original message will be saved in dead.letter.\r\n");
			state = ESM_DEADLETTER;
			break;

		  case ESM_MAIL:
			/*
			**  If mailing back, do it.
			**	Throw away all further output.  Don't alias,
			**	since this could cause loops, e.g., if joe
			**	mails to joe@x, and for some reason the network
			**	for @x is down, then the response gets sent to
			**	joe@x, which gives a response, etc.  Also force
			**	the mail to be delivered even if a version of
			**	it has already been sent to the sender.
			**
			**  If this is a configuration or local software
			**	error, send to the local postmaster as well,
			**	since the originator can't do anything
			**	about it anyway.  Note that this is a full
			**	copy of the message (intentionally) so that
			**	the Postmaster can forward things along.
			*/

			if (ExitStat == EX_CONFIG || ExitStat == EX_SOFTWARE)
			{
				(void) sendtolist("postmaster",
					  NULLADDR, &e->e_errorqueue, e);
			}
			if (strcmp(e->e_from.q_paddr, "<>") != 0)
			{
				(void) sendtolist(e->e_from.q_paddr,
					  NULLADDR, &e->e_errorqueue, e);
			}

			/*
			**  Deliver a non-delivery report to the
			**  Postmaster-designate (not necessarily
			**  Postmaster).  This does not include the
			**  body of the message, for privacy reasons.
			**  You really shouldn't need this.
			*/

			e->e_flags |= EF_PM_NOTIFY;

			/* check to see if there are any good addresses */
			for (q = e->e_errorqueue; q != NULL; q = q->q_next)
				if (!bitset(QBADADDR|QDONTSEND, q->q_flags))
					break;
			if (q == NULL)
			{
				/* this is an error-error */
				state = ESM_POSTMASTER;
				break;
			}
			if (returntosender(e->e_message, e->e_errorqueue,
					   (e->e_class >= 0), e) == 0)
			{
				state = ESM_DONE;
				break;
			}

			/* didn't work -- return to postmaster */
			state = ESM_POSTMASTER;
			break;

		  case ESM_POSTMASTER:
			/*
			**  Similar to previous case, but to system postmaster.
			*/

			q = NULL;
			if (sendtolist("postmaster", NULL, &q, e) <= 0)
			{
				syserr("553 cannot parse postmaster!");
				ExitStat = EX_SOFTWARE;
				state = ESM_USRTMP;
				break;
			}
			if (returntosender(e->e_message,
					   q, (e->e_class >= 0), e) == 0)
			{
				state = ESM_DONE;
				break;
			}

			/* didn't work -- last resort */
			state = ESM_USRTMP;
			break;

		  case ESM_DEADLETTER:
			/*
			**  Save the message in dead.letter.
			**	If we weren't mailing back, and the user is
			**	local, we should save the message in
			**	~/dead.letter so that the poor person doesn't
			**	have to type it over again -- and we all know
			**	what poor typists UNIX users are.
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
				/* no local directory */
				state = ESM_MAIL;
				break;
			}
			if (e->e_dfp != NULL)
			{
				bool oldverb = Verbose;

				/* we have a home directory; open dead.letter */
				define('z', p, e);
				expand("\201z/dead.letter", buf, &buf[sizeof buf - 1], e);
				Verbose = TRUE;
				message("Saving message in %s", buf);
				Verbose = oldverb;
				e->e_to = buf;
				q = NULL;
				(void) sendtolist(buf, &e->e_from, &q, e);
				if (q != NULL &&
				    !bitset(QBADADDR, q->q_flags) &&
				    deliver(e, q) == 0)
					state = ESM_DONE;
				else
					state = ESM_MAIL;
			}
			else
			{
				/* no data file -- try mailing back */
				state = ESM_MAIL;
			}
			break;

		  case ESM_USRTMP:
			/*
			**  Log the mail in /usr/tmp/dead.letter.
			*/

			if (e->e_class < 0)
			{
				state = ESM_DONE;
				break;
			}

			strcpy(buf, _PATH_VARTMP);
			strcat(buf, "dead.letter");
			if (!writable(buf, NULLADDR, SFF_NOSLINK))
			{
				state = ESM_PANIC;
				break;
			}
			fp = dfopen(buf, O_WRONLY|O_CREAT|O_APPEND, FileMode);
			if (fp == NULL)
			{
				state = ESM_PANIC;
				break;
			}

			bzero(&mcibuf, sizeof mcibuf);
			mcibuf.mci_out = fp;
			mcibuf.mci_mailer = FileMailer;
			if (bitnset(M_7BITS, FileMailer->m_flags))
				mcibuf.mci_flags |= MCIF_7BIT;

			putfromline(&mcibuf, e);
			(*e->e_puthdr)(&mcibuf, e);
			putline("\n", &mcibuf);
			(*e->e_putbody)(&mcibuf, e, NULL);
			putline("\n", &mcibuf);
			(void) fflush(fp);
			state = ferror(fp) ? ESM_PANIC : ESM_DONE;
			(void) xfclose(fp, "savemail", "/usr/tmp/dead.letter");
			break;

		  default:
			syserr("554 savemail: unknown state %d", state);

			/* fall through ... */

		  case ESM_PANIC:
			/* leave the locked queue & transcript files around */
			syserr("!554 savemail: cannot save rejected email anywhere");
		}
	}
}
/*
**  RETURNTOSENDER -- return a message to the sender with an error.
**
**	Parameters:
**		msg -- the explanatory message.
**		returnq -- the queue of people to send the message to.
**		sendbody -- if TRUE, also send back the body of the
**			message; otherwise just send the header.
**		e -- the current envelope.
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
#define ERRORFUDGE	100	/* nominal size of error message text */

returntosender(msg, sendbody)
	char *msg;
	bool sendbody;
	register ENVELOPE *e;
{
	char buf[MAXNAME];
	extern errhdr();

	if (tTd(6, 1))
	{
		printf("Return To Sender: msg=\"%s\", depth=%d, e=%x, returnq=",
		       msg, returndepth, e);
		printaddr(returnq, TRUE);
	}

	if (++returndepth >= MAXRETURNS)
	{
		if (returndepth != MAXRETURNS)
			syserr("554 returntosender: infinite recursion on %s", returnq->q_paddr);
		/* don't "unrecurse" and fake a clean exit */
		/* returndepth--; */
		return (0);
	}

	ErrorMessage = msg;
	SendBody = sendbody;

	/* fake up an address header for the from person */
	bmove((char *) &CurEnv->e_from, (char *) &to_addr, sizeof to_addr);
	(void) expand("$n", buf, &buf[sizeof buf - 1]);
	if (parse(buf, &CurEnv->e_from, -1) == NULL)
	{
		syserr("553 Can't parse myself!");
		ExitStat = EX_SOFTWARE;
		returndepth--;
		return (-1);
	}
	ee->e_sender = ee->e_from.q_paddr;

	/* if CurEnv->e_from was queued up, put in on CurEnv->e_sendqueue */
	if (bitset(QQUEUEUP, CurEnv->e_from.q_flags))
	{
		CurEnv->e_from.q_next = CurEnv->e_sendqueue;
		CurEnv->e_sendqueue = &CurEnv->e_from;
	}

	/* mark statistics */
	markstats(ee, NULLADDR);

	/* should check for delivery errors here */
	return (0);
}
/*
**  ERRHDR -- Output the header for error mail.
**	Parameters:
**
**	Returns:
**		none
**
**	Side Effects:
**		Outputs the current message with an appropriate
**		error header.
*/

errhdr(fp, m, xdot)
errbody(mci, e)
	register MCI *mci;
	register ENVELOPE *e;
{
	char buf[MAXLINE];
	register FILE *xfile;
	extern char *macvalue();
	char *oldfmac;
	char *oldgmac;

	if (e->e_parent == NULL)
	{
		syserr("errbody: null parent");
		putline("   ----- Original message lost -----\n", mci);
		return;
	}

	/*
	**  Output MIME header.
	*/

	if (e->e_msgboundary != NULL)
	{
		putline("This is a MIME-encapsulated message", mci);
		putline("", mci);
		(void) sprintf(buf, "--%s", e->e_msgboundary);
		putline(buf, mci);
		putline("", mci);
	}

	/*
	**  Output introductory information.
	*/

	for (q = e->e_parent->e_sendqueue; q != NULL; q = q->q_next)
		if (bitset(QBADADDR, q->q_flags))
			break;
	if (q == NULL &&
	    !bitset(EF_FATALERRS|EF_SENDRECEIPT, e->e_parent->e_flags))
	{
		putline("    **********************************************",
			mci);
		putline("    **      THIS IS A WARNING MESSAGE ONLY      **",
			mci);
		putline("    **  YOU DO NOT NEED TO RESEND YOUR MESSAGE  **",
			mci);
		putline("    **********************************************",
			mci);
		putline("", mci);
	}
	sprintf(buf, "The original message was received at %s",
		arpadate(ctime(&e->e_parent->e_ctime)));
	putline(buf, mci);
	expand("from \201_", buf, &buf[sizeof buf - 1], e->e_parent);
	putline(buf, mci);
	putline("", mci);

	/*
	**  Output error message header (if specified and available).
	*/

	if (ErrMsgFile != NULL)
	{
		if (*ErrMsgFile == '/')
		{
			xfile = fopen(ErrMsgFile, "r");
			if (xfile != NULL)
			{
				while (fgets(buf, sizeof buf, xfile) != NULL)
				{
					expand(buf, buf, &buf[sizeof buf - 1], e);
					putline(buf, mci);
				}
				(void) fclose(xfile);
				putline("\n", mci);
			}
		}
		else
		{
			expand(ErrMsgFile, buf, &buf[sizeof buf - 1], e);
			putline(buf, mci);
			putline("", mci);
		}
	}

	/*
	**  Output message introduction
	*/

	printheader = TRUE;
	for (q = e->e_parent->e_sendqueue; q != NULL; q = q->q_next)
	{
		if (bitset(QBADADDR|QREPORT, q->q_flags))
		{
			if (printheader)
			{
				putline("   ----- The following addresses had delivery problems -----",
					mci);
				printheader = FALSE;
			}
			strcpy(buf, q->q_paddr);
			if (bitset(QBADADDR, q->q_flags))
				strcat(buf, "  (unrecoverable error)");
			else
				strcat(buf, "  (transient failure)");
			putline(buf, mci);
			if (q->q_alias != NULL)
			{
				strcpy(buf, "    (expanded from: ");
				strcat(buf, q->q_alias->q_paddr);
				strcat(buf, ")");
				putline(buf, mci);
			}
		}
	}
	if (!printheader)
		putline("\n", mci);

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
		putline("   ----- Transcript of session is unavailable -----\n", mci);
	}
	else
	{
		putline("   ----- Transcript of session follows -----\n", mci);
		if (e->e_xfp != NULL)
			(void) fflush(e->e_xfp);
		while (fgets(buf, sizeof buf, xfile) != NULL)
			putline(buf, mci);
		(void) xfclose(xfile, "errbody xscript", p);
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
		SendBody = FALSE;
	putline("", mci);
	if (e->e_parent->e_df != NULL)
	{
		if (SendBody)
			putline("   ----- Original message follows -----\n", mci);
		else
			putline("   ----- Message header follows -----\n", mci);
		(void) fflush(mci->mci_out);

		if (e->e_msgboundary != NULL)
		{
			putline("", mci);
			(void) sprintf(buf, "--%s", e->e_msgboundary);
			putline(buf, mci);
			putline("Content-Type: message/rfc822", mci);
			putline("", mci);
		}
		putheader(mci, e->e_parent);
		putline("", mci);
		if (SendBody)
			putbody(mci, e->e_parent, e->e_msgboundary);
		else
			putline("   ----- Message body suppressed -----", mci);
	}
	else
	{
		putline("  ----- No message was collected -----\n", mci);
	}

	if (e->e_msgboundary != NULL)
	{
		putline("", mci);
		(void) sprintf(buf, "--%s--", e->e_msgboundary);
		putline(buf, mci);
	}
	putline("", mci);

	/*
	**  Cleanup and exit
	*/

	if (errno != 0)
		syserr("errhdr: I/O error");
}
