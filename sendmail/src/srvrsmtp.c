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

# include "sendmail.h"

#ifndef lint
#ifdef SMTP
static char sccsid[] = "@(#)srvrsmtp.c	5.27 (Berkeley) 1/18/89 (with SMTP)";
#else
static char sccsid[] = "@(#)srvrsmtp.c	5.27 (Berkeley) 1/18/89 (without SMTP)";
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
	"help",		CMDHELP,
	"noop",		CMDNOOP,
	"quit",		CMDQUIT,
	"helo",		CMDHELO,
	"verb",		CMDVERB,
	"onex",		CMDONEX,
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

smtp()
{
	register char *p;
	register struct cmd *c;
	char *cmd;
	extern char *skipword();
	bool hasmail;			/* mail command received */
	auto ADDRESS *vrfyqueue;
	ADDRESS *a;
	char *sendinghost;
	char inp[MAXLINE];
	char cmdbuf[100];
	extern char Version[];
	extern char *macvalue();
	extern ADDRESS *recipient();
	extern ENVELOPE BlankEnvelope;
	extern ENVELOPE *newenvelope();

	hasmail = FALSE;
	if (OutChannel != stdout)
	{
		/* arrange for debugging output to go to remote host */
		(void) close(1);
		(void) dup(fileno(OutChannel));
	}
	settime();
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
	expand("\001e", inp, &inp[sizeof inp], CurEnv);
	message("220", inp);
	SmtpPhase = "startup";
	sendinghost = NULL;
	for (;;)
	{
		/* arrange for backout */
		if (setjmp(TopFrame) > 0 && InChild)
			finis();
		QuickAbort = FALSE;
		HoldErrs = FALSE;

		/* setup for the read */
		CurEnv->e_to = NULL;
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
			finis();
		}

		/* clean up end of line */
		fixcrlf(inp, TRUE);

		/* echo command to transcript */
		if (CurEnv->e_xfp != NULL)
			fprintf(CurEnv->e_xfp, "<<< %s\n", inp);

		/* break off command */
		for (p = inp; isspace(*p); p++)
			continue;
		cmd = p;
		for (cmd = cmdbuf; *p != '\0' && !isspace(*p); )
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
				message("553", "Local configuration error, hostname not recognized as local");
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
			message("250", "%s Hello %s, pleased to meet you",
				MyHostName, sendinghost);
			break;

		  case CMDMAIL:		/* mail -- designate sender */
			SmtpPhase = "MAIL";

			/* force a sending host even if no HELO given */
			if (RealHostName != NULL && macvalue('s', CurEnv) == NULL)
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
			if (runinchild("SMTP-MAIL") > 0)
				break;
			define('s', sendinghost, CurEnv);
			define('r', "SMTP", CurEnv);
			initsys();
			setproctitle("%s %s: %s", CurEnv->e_id,
				CurHostName, inp);

			/* child -- go do the processing */
			p = skipword(p, "from");
			if (p == NULL)
				break;
			setsender(p);
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
			setproctitle("%s %s: %s", CurEnv->e_id,
				CurHostName, inp);
			if (setjmp(TopFrame) > 0)
			{
				CurEnv->e_flags &= ~EF_FATALERRS;
				break;
			}
			QuickAbort = TRUE;
			p = skipword(p, "to");
			if (p == NULL)
				break;
			a = parseaddr(p, (ADDRESS *) NULL, 1, '\0');
			if (a == NULL)
				break;
			a->q_flags |= QPRIMARY;
			a = recipient(a, &CurEnv->e_sendqueue);
			if (Errors != 0)
				break;

			/* no errors during parsing, but might be a duplicate */
			CurEnv->e_to = p;
			if (!bitset(QBADADDR, a->q_flags))
				message("250", "Recipient ok");
			else
			{
				/* punt -- should keep message in ADDRESS.... */
				message("550", "Addressee unknown");
			}
			CurEnv->e_to = NULL;
			break;

		  case CMDDATA:		/* data -- text of mail */
			SmtpPhase = "DATA";
			if (!hasmail)
			{
				message("503", "Need MAIL command");
				break;
			}
			else if (CurEnv->e_nrcpts <= 0)
			{
				message("503", "Need RCPT (recipient)");
				break;
			}

			/* collect the text of the message */
			SmtpPhase = "collect";
			setproctitle("%s %s: %s", CurEnv->e_id,
				CurHostName, inp);
			collect(TRUE);
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
			if (CurEnv->e_nrcpts != 1)
			{
				HoldErrs = TRUE;
				ErrorMode = EM_MAIL;
			}
			CurEnv->e_flags &= ~EF_FATALERRS;
			CurEnv->e_xfp = freopen(queuename(CurEnv, 'x'), "w", CurEnv->e_xfp);

			/* send to all recipients */
			sendall(CurEnv, SM_DEFAULT);
			CurEnv->e_to = NULL;

			/* save statistics */
			markstats(CurEnv, (ADDRESS *) NULL);

			/* issue success if appropriate and reset */
			if (Errors == 0 || HoldErrs)
				message("250", "Ok");
			else
				CurEnv->e_flags &= ~EF_FATALERRS;

			/* if in a child, pop back to our parent */
			if (InChild)
				finis();

			/* clean up a bit */
			hasmail = 0;
			dropenvelope(CurEnv);
			CurEnv = newenvelope(CurEnv);
			CurEnv->e_flags = BlankEnvelope.e_flags;
			break;

		  case CMDRSET:		/* rset -- reset state */
			message("250", "Reset state");
			if (InChild)
				finis();
			break;

		  case CMDVRFY:		/* vrfy -- verify address */
			if (runinchild("SMTP-VRFY") > 0)
				break;
			setproctitle("%s: %s", CurHostName, inp);
			vrfyqueue = NULL;
			QuickAbort = TRUE;
			sendtolist(p, (ADDRESS *) NULL, &vrfyqueue);
			if (Errors != 0)
			{
				if (InChild)
					finis();
				break;
			}
			while (vrfyqueue != NULL)
			{
				register ADDRESS *a = vrfyqueue->q_next;
				char *code;

				while (a != NULL && bitset(QDONTSEND|QBADADDR, a->q_flags))
					a = a->q_next;

				if (!bitset(QDONTSEND|QBADADDR, vrfyqueue->q_flags))
				{
					if (a != NULL)
						code = "250-";
					else
						code = "250";
					if (vrfyqueue->q_fullname == NULL)
						message(code, "<%s>", vrfyqueue->q_paddr);
					else
						message(code, "%s <%s>",
						    vrfyqueue->q_fullname, vrfyqueue->q_paddr);
				}
				else if (a == NULL)
					message("554", "Self destructive alias loop");
				vrfyqueue = a;
			}
			if (InChild)
				finis();
			break;

		  case CMDHELP:		/* help -- give user info */
			if (*p == '\0')
				p = "SMTP";
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
			printaddr(CurEnv->e_sendqueue, TRUE);
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

	len = strlen(topic);
	makelower(topic);
	noinfo = TRUE;

	while (fgets(buf, sizeof buf, hf) != NULL)
	{
		if (strncmp(buf, topic, len) == 0)
		{
			register char *p;

			p = index(buf, '\t');
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

runinchild(label)
	char *label;
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
			clearenvelope(CurEnv, FALSE);
		}
	}

	/* open alias database */
	initaliases(AliasFile, FALSE);

	return (0);
}

# endif SMTP
