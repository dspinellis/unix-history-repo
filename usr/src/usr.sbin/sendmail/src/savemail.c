/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)savemail.c	6.14 (Berkeley) %G%";
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
			syserr("553 Cannot parse root!");
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
		syserr("554 savemail: ErrorMode x%x\n");
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
			**
			**	Clever technique for computing rpath from
			**	Eric Wassenaar <e07@nikhef.nl>.
			*/

			if (state == ESM_MAIL)
			{
				if (e->e_errorqueue == NULL)
				{
					char *rpath;

					if (e->e_returnpath != e->e_sender)
						rpath = e->e_returnpath;
					else
						rpath = e->e_from.q_paddr;
					if (strcmp(rpath, "<>") != 0)
						(void) sendtolist(rpath,
							  (ADDRESS *) NULL,
							  &e->e_errorqueue, e);
				}

				/* deliver a cc: to the postmaster if desired */
				if (PostMasterCopy != NULL)
					(void) sendtolist(PostMasterCopy,
							  (ADDRESS *) NULL,
							  &e->e_errorqueue, e);
				q = e->e_errorqueue;
				if (q == NULL)
				{
					/* this is an error-error */
					state = ESM_USRTMP;
					break;
				}
			}
			else
			{
				if (parseaddr("postmaster", q, 0, '\0', e) == NULL)
				{
					syserr("553 cannot parse postmaster!");
					ExitStat = EX_SOFTWARE;
					state = ESM_USRTMP;
					break;
				}
			}
			if (returntosender(e->e_message != NULL ? e->e_message :
					   "Unable to deliver mail",
					   q, (e->e_class >= 0), e) == 0)
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
				syserr("554 Can't return mail to %s", e->e_from.q_paddr);
				state = ESM_MAIL;
				break;
			}
			if (e->e_dfp != NULL)
			{
				auto ADDRESS *q;
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

			if (e->e_class < 0)
			{
				state = ESM_DONE;
				break;
			}

			fp = dfopen("/usr/tmp/dead.letter", "a");
			if (fp == NULL)
			{
				state = ESM_PANIC;
				break;
			}

			putfromline(fp, FileMailer, e);
			(*e->e_puthdr)(fp, FileMailer, e);
			putline("\n", fp, FileMailer);
			(*e->e_putbody)(fp, FileMailer, e);
			putline("\n", fp, FileMailer);
			(void) fflush(fp);
			state = ferror(fp) ? ESM_PANIC : ESM_DONE;
			(void) fclose(fp);
			break;

		  default:
			syserr("554 savemail: unknown state %d", state);

			/* fall through ... */

		  case ESM_PANIC:
			/* leave the locked queue & transcript files around */
			syserr("554 savemail: cannot save rejected email anywhere");
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

returntosender(msg, returnq, sendbody, e)
	char *msg;
	ADDRESS *returnq;
	bool sendbody;
	register ENVELOPE *e;
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
		printf("Return To Sender: msg=\"%s\", depth=%d, e=%x,\n",
		       msg, returndepth, e);
		printf("\treturnq=");
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

	SendBody = sendbody;
	define('g', e->e_sender, e);
	define('<', e->e_returnpath, e);
	ee = newenvelope(&errenvelope, e);
	define('a', "\201b", ee);
	ee->e_puthdr = putheader;
	ee->e_putbody = errbody;
	ee->e_flags |= EF_RESPONSE;
	if (!bitset(EF_OLDSTYLE, e->e_flags))
		ee->e_flags &= ~EF_OLDSTYLE;
	ee->e_sendqueue = returnq;
	openxscript(ee);
	for (q = returnq; q != NULL; q = q->q_next)
	{
		if (!DontPruneRoutes && pruneroute(q->q_paddr))
			parseaddr(q->q_paddr, q, 0, '\0', e);

		if (q->q_alias == NULL)
			addheader("to", q->q_paddr, ee);
	}

# ifdef LOG
	if (LogLevel > 5)
		syslog(LOG_INFO, "%s: %s: return to sender: %s",
			e->e_id, ee->e_id, msg);
# endif

	(void) sprintf(buf, "Returned mail: %s", msg);
	addheader("subject", buf, ee);

	/* fake up an address header for the from person */
	expand("\201n", buf, &buf[sizeof buf - 1], e);
	ee->e_sender = newstr(buf);
	if (ConfigLevel >= 4)
		ee->e_returnpath = "<>";
	else
		ee->e_returnpath = ee->e_sender;
	if (parseaddr(buf, &ee->e_from, -1, '\0', e) == NULL)
	{
		syserr("553 Can't parse myself!");
		ExitStat = EX_SOFTWARE;
		returndepth--;
		return (-1);
	}
	loweraddr(&ee->e_from);

	/* push state into submessage */
	CurEnv = ee;
	define('f', "\201n", ee);
	define('x', "Mail Delivery Subsystem", ee);
	eatheader(ee, FALSE);

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
					putline(buf, fp, m);
				}
				(void) fclose(xfile);
				fprintf(fp, "\n");
			}
		}
		else
		{
			expand(ErrMsgFile, buf, &buf[sizeof buf - 1], e);
			putline(buf, fp, m);
			fprintf(fp, "\n");
		}
	}

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
/*
**  PRUNEROUTE -- prune an RFC-822 source route
** 
**	Trims down a source route to the last internet-registered hop.
**	This is encouraged by RFC 1123 section 5.3.3.
** 
**	Parameters:
**		addr -- the address
** 
**	Returns:
**		TRUE -- address was modified
**		FALSE -- address could not be pruned
** 
**	Side Effects:
**		modifies addr in-place
*/

pruneroute(addr)
	char *addr;
{
#ifdef NAMED_BIND
	char *start, *at, *comma;
	char c;
	int rcode;
	char hostbuf[BUFSIZ];
	char *mxhosts[MAXMXHOSTS + 1];

	/* check to see if this is really a route-addr */
	if (*addr != '<' || addr[1] != '@' || addr[strlen(addr) - 1] != '>')
		return FALSE;
	start = strchr(addr, ':');
	at = strrchr(addr, '@');
	if (start == NULL || at == NULL || at < start)
		return FALSE;

	/* slice off the angle brackets */
	strcpy(hostbuf, at + 1);
	hostbuf[strlen(hostbuf) - 1] = '\0';

	while (start)
	{
		if (getmxrr(hostbuf, mxhosts, "", &rcode) > 0)
		{
			strcpy(addr + 1, start + 1);
			return TRUE;
		}
		c = *start;
		*start = '\0';
		comma = strrchr(addr, ',');
		if (comma && comma[1] == '@')
			strcpy(hostbuf, comma + 2);
		else
			comma = 0;
		*start = c;
		start = comma;
	}
#endif
	return FALSE;
}
