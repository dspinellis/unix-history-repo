/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)savemail.c	5.12 (Berkeley) %G%";
#endif /* not lint */

# include <sys/types.h>
# include <pwd.h>
# include "sendmail.h"

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


savemail(e)
	register ENVELOPE *e;
{
	register struct passwd *pw;
	register FILE *fp;
	int state;
	auto ADDRESS *q;
	char buf[MAXLINE+1];
	extern struct passwd *getpwnam();
	register char *p;
	extern char *ttypath();
	typedef int (*fnptr)();

	if (tTd(6, 1))
		printf("\nsavemail, ErrorMode = %c\n", ErrorMode);

	if (bitset(EF_RESPONSE, e->e_flags))
		return;
	if (e->e_class < 0)
	{
		message(Arpa_Info, "Dumping junk mail");
		return;
	}
	ForceMail = TRUE;
	e->e_flags &= ~EF_FATALERRS;

	/*
	**  In the unhappy event we don't know who to return the mail
	**  to, make someone up.
	*/

	if (CurEnv->e_returnto == NULL)
	{
		CurEnv->e_returnto = parse("root", (ADDRESS *) NULL, 0);
		if (CurEnv->e_returnto == NULL)
		{
			syserr("Cannot parse root!");
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
	switch (ErrorMode)
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
		syserr("savemail: ErrorMode x%x\n");
		state = ESM_MAIL;
		break;
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

			expand("\001n", buf, &buf[sizeof buf - 1], e);
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
				syserr("Cannot open %s", queuename(e, 'x'));
				printf("Transcript of session is unavailable.\r\n");
			}
			else
			{
				printf("Transcript follows:\r\n");
				while (fgets(buf, sizeof buf, fp) != NULL &&
				       !ferror(stdout))
					fputs(buf, stdout);
				(void) fclose(fp);
			}
			printf("Original message will be saved in dead.letter.\r\n");
			state = ESM_DEADLETTER;
			break;

		  case ESM_MAIL:
		  case ESM_POSTMASTER:
			/*
			**  If mailing back, do it.
			**	Throw away all further output.  Don't alias,
			**	since this could cause loops, e.g., if joe
			**	mails to joe@x, and for some reason the network
			**	for @x is down, then the response gets sent to
			**	joe@x, which gives a response, etc.  Also force
			**	the mail to be delivered even if a version of
			**	it has already been sent to the sender.
			*/

			if (state == ESM_MAIL)
			{
				if (e->e_errorqueue == NULL)
					sendtolist(e->e_from.q_paddr,
						(ADDRESS *) NULL,
						&e->e_errorqueue);

				/* deliver a cc: to the postmaster if desired */
				if (PostMasterCopy != NULL)
					sendtolist(PostMasterCopy,
						(ADDRESS *) NULL,
						&e->e_errorqueue);
				q = e->e_errorqueue;
			}
			else
			{
				if (parseaddr("postmaster", q, 0, '\0') == NULL)
				{
					syserr("cannot parse postmaster!");
					ExitStat = EX_SOFTWARE;
					state = ESM_USRTMP;
					break;
				}
			}
			if (returntosender(e->e_message != NULL ? e->e_message :
					   "Unable to deliver mail",
					   q, TRUE) == 0)
			{
				state = ESM_DONE;
				break;
			}

			state = state == ESM_MAIL ? ESM_POSTMASTER : ESM_USRTMP;
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
				syserr("Can't return mail to %s", e->e_from.q_paddr);
				state = ESM_MAIL;
				break;
			}
			if (e->e_dfp != NULL)
			{
				auto ADDRESS *q;
				bool oldverb = Verbose;

				/* we have a home directory; open dead.letter */
				define('z', p, e);
				expand("\001z/dead.letter", buf, &buf[sizeof buf - 1], e);
				Verbose = TRUE;
				message(Arpa_Info, "Saving message in %s", buf);
				Verbose = oldverb;
				e->e_to = buf;
				q = NULL;
				sendtolist(buf, (ADDRESS *) NULL, &q);
				if (deliver(e, q) == 0)
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

			fp = dfopen("/usr/tmp/dead.letter", "a");
			if (fp == NULL)
			{
				state = ESM_PANIC;
				break;
			}

			putfromline(fp, ProgMailer);
			(*e->e_puthdr)(fp, ProgMailer, e);
			putline("\n", fp, ProgMailer);
			(*e->e_putbody)(fp, ProgMailer, e);
			putline("\n", fp, ProgMailer);
			(void) fflush(fp);
			state = ferror(fp) ? ESM_PANIC : ESM_DONE;
			(void) fclose(fp);
			break;

		  default:
			syserr("savemail: unknown state %d", state);

			/* fall through ... */

		  case ESM_PANIC:
			syserr("savemail: HELP!!!!");
# ifdef LOG
			if (LogLevel >= 1)
				syslog(LOG_ALERT, "savemail: HELP!!!!");
# endif LOG

			/* leave the locked queue & transcript files around */
			exit(EX_SOFTWARE);
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

returntosender(msg, returnq, sendbody)
	char *msg;
	ADDRESS *returnq;
	bool sendbody;
{
	char buf[MAXNAME];
	extern putheader(), errbody();
	register ENVELOPE *ee;
	extern ENVELOPE *newenvelope();
	ENVELOPE errenvelope;
	static int returndepth;
	register ADDRESS *q;

	if (tTd(6, 1))
	{
		printf("Return To Sender: msg=\"%s\", depth=%d, CurEnv=%x,\n",
		       msg, returndepth, CurEnv);
		printf("\treturnq=");
		printaddr(returnq, TRUE);
	}

	if (++returndepth >= MAXRETURNS)
	{
		if (returndepth != MAXRETURNS)
			syserr("returntosender: infinite recursion on %s", returnq->q_paddr);
		/* don't "unrecurse" and fake a clean exit */
		/* returndepth--; */
		return (0);
	}

	SendBody = sendbody;
	define('g', "\001f", CurEnv);
	ee = newenvelope(&errenvelope);
	define('a', "\001b", ee);
	ee->e_puthdr = putheader;
	ee->e_putbody = errbody;
	ee->e_flags |= EF_RESPONSE;
	ee->e_sendqueue = returnq;
	openxscript(ee);
	for (q = returnq; q != NULL; q = q->q_next)
	{
		if (q->q_alias == NULL)
			addheader("to", q->q_paddr, ee);
	}

	(void) sprintf(buf, "Returned mail: %s", msg);
	addheader("subject", buf, ee);

	/* fake up an address header for the from person */
	expand("\001n", buf, &buf[sizeof buf - 1], CurEnv);
	if (parseaddr(buf, &ee->e_from, -1, '\0') == NULL)
	{
		syserr("Can't parse myself!");
		ExitStat = EX_SOFTWARE;
		returndepth--;
		return (-1);
	}
	loweraddr(&ee->e_from);

	/* push state into submessage */
	CurEnv = ee;
	define('f', "\001n", ee);
	define('x', "Mail Delivery Subsystem", ee);
	eatheader(ee);

	/* actually deliver the error message */
	sendall(ee, SM_DEFAULT);

	/* restore state */
	dropenvelope(ee);
	CurEnv = CurEnv->e_parent;
	returndepth--;

	/* should check for delivery errors here */
	return (0);
}
/*
**  ERRBODY -- output the body of an error message.
**
**	Typically this is a copy of the transcript plus a copy of the
**	original offending message.
**
**	Parameters:
**		fp -- the output file.
**		m -- the mailer to output to.
**		e -- the envelope we are working in.
**
**	Returns:
**		none
**
**	Side Effects:
**		Outputs the body of an error message.
*/

errbody(fp, m, e)
	register FILE *fp;
	register struct mailer *m;
	register ENVELOPE *e;
{
	register FILE *xfile;
	char buf[MAXLINE];
	char *p;

	/*
	**  Output transcript of errors
	*/

	(void) fflush(stdout);
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
			putheader(fp, m, e->e_parent);
			putline("\n", fp, m);
			putbody(fp, m, e->e_parent);
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
		syserr("errbody: I/O error");
}
