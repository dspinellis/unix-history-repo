/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

# include "sendmail.h"

#ifndef lint
#ifdef SMTP
static char sccsid[] = "@(#)srvrsmtp.c	6.1 (Berkeley) %G% (with SMTP)";
#else
static char sccsid[] = "@(#)srvrsmtp.c	6.1 (Berkeley) %G% (without SMTP)";
#endif
#endif /* not lint */

# include <errno.h>
# include <signal.h>

# ifdef SMTP

/*
**  SMTP -- run the SMTP protocol.
**
**	Parameters:
**		none.
**
**	Returns:
**		never.
**
**	Side Effects:
**		Reads commands from the input channel and processes
**			them.
*/

struct cmd
{
	char	*cmdname;	/* command name */
	int	cmdcode;	/* internal code, see below */
};

/* values for cmdcode */
# define CMDERROR	0	/* bad command */
# define CMDMAIL	1	/* mail -- designate sender */
# define CMDRCPT	2	/* rcpt -- designate recipient */
# define CMDDATA	3	/* data -- send message text */
# define CMDHOPS	4	/* hops -- specify hop count */
# define CMDRSET	4	/* rset -- reset state */
# define CMDVRFY	5	/* vrfy -- verify address */
# define CMDHELP	6	/* help -- give usage info */
# define CMDNOOP	7	/* noop -- do nothing */
# define CMDQUIT	8	/* quit -- close connection and die */
# define CMDHELO	9	/* helo -- be polite */
# define CMDONEX	10	/* onex -- sending one transaction only */
# define CMDVERB	11	/* verb -- go into verbose mode */
/* debugging-only commands, only enabled if SMTPDEBUG is defined */
# define CMDDBGQSHOW	12	/* showq -- show send queue */
# define CMDDBGDEBUG	13	/* debug -- set debug mode */

static struct cmd	CmdTab[] =
{
	"mail",		CMDMAIL,
	"rcpt",		CMDRCPT,
	"data",		CMDDATA,
	"rset",		CMDRSET,
	"vrfy",		CMDVRFY,
	"expn",		CMDVRFY,
	"expn",		CMDVRFY,
	"help",		CMDHELP,
	"noop",		CMDNOOP,
	"quit",		CMDQUIT,
	"helo",		CMDHELO,
	"verb",		CMDVERB,
	"onex",		CMDONEX,
	"hops",		CMDHOPS,
	/*
	 * remaining commands are here only
	 * to trap and log attempts to use them
	 */
	"showq",	CMDDBGQSHOW,
	"debug",	CMDDBGDEBUG,
	NULL,		CMDERROR,
};

bool	InChild = FALSE;		/* true if running in a subprocess */
bool	OneXact = FALSE;		/* one xaction only this run */

#define EX_QUIT		22		/* special code for QUIT command */

smtp(e)
	register ENVELOPE *e;
{
	register char *p;
	register struct cmd *c;
	char *cmd;
	static char *skipword();
	bool hasmail;			/* mail command received */
	extern ADDRESS *sendto();
	ADDRESS *a;

	hasmail = FALSE;
	if (OutChannel != stdout)
	{
		/* arrange for debugging output to go to remote host */
		(void) close(1);
		(void) dup(fileno(OutChannel));
	}
	settime(e);
	if (RealHostName != NULL)
	{
		CurHostName = RealHostName;
		setproctitle("srvrsmtp %s", CurHostName);
	}
	else
	{
		/* this must be us!! */
		CurHostName = MyHostName;
	}
	expand("\001e", inp, &inp[sizeof inp], e);
	message("220", "%s", inp);
	SmtpPhase = "startup";
	sendinghost = NULL;
	for (;;)
	{
		/* arrange for backout */
		if (setjmp(TopFrame) > 0 && InChild)
			finis();
		QuickAbort = FALSE;
		HoldErrs = FALSE;
		LogUsrErrs = FALSE;

		/* setup for the read */
		e->e_to = NULL;
		Errors = 0;
		(void) fflush(stdout);

		/* read the input line */
		p = sfgets(inp, sizeof inp, InChannel);

		/* handle errors */
		if (p == NULL)
		{
			/* end of file, just die */
			message("421", "%s Lost input channel from %s",
				MyHostName, CurHostName);
#ifdef LOG
			if (LogLevel >= 4)
				syslog(LOG_NOTICE, "lost input channel from %s",
					CurHostName);
#endif
			finis();
		}

		/* clean up end of line */
		fixcrlf(inp, TRUE);

		/* echo command to transcript */
		if (e->e_xfp != NULL)
			fprintf(e->e_xfp, "<<< %s\n", inp);

		/* break off command */
		for (p = inp; isspace(*p); p++)
			continue;
		cmd = cmdbuf;
		while (*p != '\0' && !isspace(*p) && cmd < &cmdbuf[sizeof cmdbuf - 2])
			*cmd++ = *p++;
		*cmd = '\0';

		/* throw away leading whitespace */
		while (isspace(*p))
			p++;

		/* decode command */
		for (c = CmdTab; c->cmdname != NULL; c++)
		{
			if (!strcasecmp(c->cmdname, cmdbuf))
				break;
		}

		/* reset errors */
		errno = 0;

		/* process command */
		switch (c->cmdcode)
		{
		  case CMDHELO:		/* hello -- introduce yourself */
			SmtpPhase = "HELO";
			setproctitle("%s: %s", CurHostName, inp);
			if (!strcasecmp(p, MyHostName))
			{
				/*
				 * didn't know about alias,
				 * or connected to an echo server
				 */
				message("553", "%s config error: mail loops back to myself",
					MyHostName);
				break;
			}
			if (RealHostName != NULL && strcasecmp(p, RealHostName))
			{
				char hostbuf[MAXNAME];

				(void) sprintf(hostbuf, "%s (%s)", p, RealHostName);
				sendinghost = newstr(hostbuf);
			}
			else
				sendinghost = newstr(p);
			message("250", "%s Hello %s, pleased to meet you", HostName, p);
			break;

		  case CMDMAIL:		/* mail -- designate sender */
			SmtpPhase = "MAIL";

			/* force a sending host even if no HELO given */
			if (RealHostName != NULL && macvalue('s', e) == NULL)
				sendinghost = RealHostName;

			/* check for validity of this command */
			if (hasmail)
			{
				message("503", "Sender already specified");
				break;
			}
			if (InChild)
			{
				errno = 0;
				syserr("Nested MAIL command");
				exit(0);
			}

			/* fork a subprocess to process this command */
			if (runinchild("SMTP-MAIL", e) > 0)
				break;
			define('s', sendinghost, e);
			define('r', "SMTP", e);
			initsys(e);
			setproctitle("%s %s: %s", e->e_id,
				CurHostName, inp);

			/* child -- go do the processing */
			p = skipword(p, "from");
			if (p == NULL)
				break;
			setsender(p, e);
			if (Errors == 0)
			{
				message("250", "Sender ok");
				hasmail = TRUE;
			}
			else if (InChild)
				finis();
			break;

		  case CMDRCPT:		/* rcpt -- designate recipient */
			SmtpPhase = "RCPT";
			setproctitle("%s %s: %s", e->e_id,
				CurHostName, inp);
			if (setjmp(TopFrame) > 0)
			{
				e->e_flags &= ~EF_FATALERRS;
				break;
			}
			QuickAbort = TRUE;
			LogUsrErrs = TRUE;
			p = skipword(p, "to");
			if (p == NULL)
				break;
			a = sendto(p, 1, (ADDRESS *) NULL, 0);
# ifdef DEBUG
			if (Debug > 1)
				printaddr(a, TRUE);
# endif DEBUG
			if (Errors != 0)
				break;

			/* no errors during parsing, but might be a duplicate */
			e->e_to = p;
			if (!bitset(QBADADDR, a->q_flags))
				message("250", "Recipient ok");
			else
			{
				/* punt -- should keep message in ADDRESS.... */
				message("550", "Addressee unknown");
			}
			e->e_to = NULL;
			break;

		  case CMDDATA:		/* data -- text of mail */
			SmtpPhase = "DATA";
			if (!hasmail)
			{
				message("503", "Need MAIL command");
				break;
			}
			else if (e->e_nrcpts <= 0)
			{
				message("503", "Need RCPT (recipient)");
				break;
			}

			/* collect the text of the message */
			SmtpPhase = "collect";
			setproctitle("%s %s: %s", e->e_id,
				CurHostName, inp);
			collect(TRUE, e);
			if (Errors != 0)
				break;

			/*
			**  Arrange to send to everyone.
			**	If sending to multiple people, mail back
			**		errors rather than reporting directly.
			**	In any case, don't mail back errors for
			**		anything that has happened up to
			**		now (the other end will do this).
			**	Truncate our transcript -- the mail has gotten
			**		to us successfully, and if we have
			**		to mail this back, it will be easier
			**		on the reader.
			**	Then send to everyone.
			**	Finally give a reply code.  If an error has
			**		already been given, don't mail a
			**		message back.
			**	We goose error returns by clearing error bit.
			*/

			SmtpPhase = "delivery";
			if (e->e_nrcpts != 1)
			{
				HoldErrs = TRUE;
				ErrorMode = EM_MAIL;
			}
			e->e_flags &= ~EF_FATALERRS;
			e->e_xfp = freopen(queuename(e, 'x'), "w", e->e_xfp);

			/* send to all recipients */
			sendall(e, SM_DEFAULT);
			e->e_to = NULL;

			/* save statistics */
			markstats(e, (ADDRESS *) NULL);

			/* issue success if appropriate and reset */
			if (Errors == 0 || HoldErrs)
				message("250", "Ok");
			else
				e->e_flags &= ~EF_FATALERRS;

			/* if in a child, pop back to our parent */
			if (InChild)
				finis();

			/* clean up a bit */
			hasmail = 0;
			dropenvelope(e);
			CurEnv = e = newenvelope(e);
			e->e_flags = BlankEnvelope.e_flags;
			break;

		  case CMDRSET:		/* rset -- reset state */
			message("250", "Reset state");
			if (InChild)
				finis();
			break;

		  case CMDVRFY:		/* vrfy -- verify address */
			if (runinchild("SMTP-VRFY", e) > 0)
				break;
			setproctitle("%s: %s", CurHostName, inp);
#ifdef LOG
			if (LogLevel >= 9)
				syslog(LOG_INFO, "%s: %s", CurHostName, inp);
#endif
				paddrtree(a);
			break;

		  case CMDHELP:		/* help -- give user info */
			help(p);
			break;

		  case CMDNOOP:		/* noop -- do nothing */
			message("200", "OK");
			break;

		  case CMDQUIT:		/* quit -- leave mail */
			message("221", "%s closing connection", MyHostName);
			if (InChild)
				ExitStat = EX_QUIT;
			finis();

		  case CMDVERB:		/* set verbose mode */
			Verbose = TRUE;
			SendMode = SM_DELIVER;
			message("200", "Verbose mode");
			break;

		  case CMDONEX:		/* doing one transaction only */
			OneXact = TRUE;
			message("200", "Only one transaction");
			break;

# ifdef SMTPDEBUG
		  case CMDDBGQSHOW:	/* show queues */
			printf("Send Queue=");
			printaddr(e->e_sendqueue, TRUE);
			break;

		  case CMDDBGDEBUG:	/* set debug mode */
			tTsetup(tTdvect, sizeof tTdvect, "0-99.1");
			tTflag(p);
			message("200", "Debug set");
			break;

# else /* not SMTPDEBUG */

		  case CMDDBGQSHOW:	/* show queues */
		  case CMDDBGDEBUG:	/* set debug mode */
# ifdef LOG
			if (RealHostName != NULL && LogLevel > 0)
				syslog(LOG_NOTICE,
				    "\"%s\" command from %s (%s)\n",
				    c->cmdname, RealHostName,
				    inet_ntoa(RealHostAddr.sin_addr));
# endif
			/* FALL THROUGH */
# endif /* SMTPDEBUG */

		  case CMDERROR:	/* unknown command */
			message("500", "Command unrecognized");
			break;

		  default:
			errno = 0;
			syserr("smtp: unknown code %d", c->cmdcode);
			break;
		}
	}
}
/*
**  SKIPWORD -- skip a fixed word.
**
**	Parameters:
**		p -- place to start looking.
**		w -- word to skip.
**
**	Returns:
**		p following w.
**		NULL on error.
**
**	Side Effects:
**		clobbers the p data area.
*/

static char *
skipword(p, w)
	register char *p;
	char *w;
{
	register char *q;

	/* find beginning of word */
	while (isspace(*p))
		p++;
	q = p;

	/* find end of word */
	while (*p != '\0' && *p != ':' && !isspace(*p))
		p++;
	while (isspace(*p))
		*p++ = '\0';
	if (*p != ':')
	{
	  syntax:
		message("501", "Syntax error");
		Errors++;
		return (NULL);
	}
	*p++ = '\0';
	while (isspace(*p))
		p++;

	/* see if the input word matches desired word */
	if (strcasecmp(q, w))
		goto syntax;

	return (p);
}
/*
**  HELP -- implement the HELP command.
**
**	Parameters:
**		topic -- the topic we want help for.
**
**	Returns:
**		none.
**
**	Side Effects:
**		outputs the help file to message output.
*/

help(topic)
	char *topic;
{
	register FILE *hf;
	int len;
	char buf[MAXLINE];
	bool noinfo;

	if (HelpFile == NULL || (hf = fopen(HelpFile, "r")) == NULL)
	{
		/* no help */
		errno = 0;
		message("502", "HELP not implemented");
		return;
	}

	if (topic == NULL || *topic == '\0')
		topic = "smtp";
	else
		makelower(topic);

	len = strlen(topic);
	noinfo = TRUE;

	while (fgets(buf, sizeof buf, hf) != NULL)
	{
		if (strncmp(buf, topic, len) == 0)
		{
			register char *p;

			p = strchr(buf, '\t');
			if (p == NULL)
				p = buf;
			else
				p++;
			fixcrlf(p, TRUE);
			message("214-", p);
			noinfo = FALSE;
		}
	}

	if (noinfo)
		message("504", "HELP topic unknown");
	else
		message("214", "End of HELP info");
	(void) fclose(hf);
}
/*
**  RUNINCHILD -- return twice -- once in the child, then in the parent again
**
**	Parameters:
**		label -- a string used in error messages
**
**	Returns:
**		zero in the child
**		one in the parent
**
**	Side Effects:
**		none.
*/

runinchild(label, e)
	char *label;
	register ENVELOPE *e;
{
	int childpid;

	if (!OneXact)
	{
		childpid = dofork();
		if (childpid < 0)
		{
			syserr("%s: cannot fork", label);
			return (1);
		}
		if (childpid > 0)
		{
			auto int st;

			/* parent -- wait for child to complete */
			st = waitfor(childpid);
			if (st == -1)
				syserr("%s: lost child", label);

			/* if we exited on a QUIT command, complete the process */
			if (st == (EX_QUIT << 8))
				finis();

			return (1);
		}
		else
		{
			/* child */
			InChild = TRUE;
			QuickAbort = FALSE;
			clearenvelope(e, FALSE);
		}
	}

	/* open alias database */
	initaliases(AliasFile, FALSE, e);

	return (0);
}
/*
**  PADDRTREE -- print address tree
**
**	Used by VRFY and EXPD to dump the tree of addresses produced.
**
**	Parameters:
**		a -- address of root.
**
**	Returns:
**		none.
**
**	Side Effects:
**		prints the tree in a nice order.
*/

paddrtree(a)
	register ADDRESS *a;
{
	static ADDRESS *prev;
	static int lev;

	if (a == NULL)
		return;
	lev++;
	if (!bitset(QDONTSEND, a->q_flags))
	{
		if (prev != NULL)
		{
			if (prev->q_fullname != NULL)
				message("250-", "%s <%s>", prev->q_fullname, prev->q_paddr);
			else
				message("250-", "<%s>", prev->q_paddr);
		}
		prev = a;
	}
	paddrtree(a->q_child);
	paddrtree(a->q_sibling);
	if (--lev <= 0)
	{
		if (prev != NULL)
		{
			/* last one */
			if (prev->q_fullname != NULL)
				message("250", "%s <%s>", prev->q_fullname, prev->q_paddr);
			else
				message("250", "<%s>", prev->q_paddr);
			prev = NULL;
		}
		else
			message("550", "User unknown");
	}
}

# endif /* SMTP */
