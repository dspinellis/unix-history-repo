/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
*/


# include <errno.h>
# include "sendmail.h"
# include <signal.h>

# ifndef SMTP
static char	SccsId[] = "@(#)srvrsmtp.c	5.3 (Berkeley) %G%	(no SMTP)";
# else SMTP

static char	SccsId[] = "@(#)srvrsmtp.c	5.3 (Berkeley) %G%";

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
# define CMDDBGQSHOW	10	/* showq -- show send queue (DEBUG) */
# define CMDDBGDEBUG	11	/* debug -- set debug mode */
# define CMDVERB	12	/* verb -- go into verbose mode */
# define CMDDBGKILL	13	/* kill -- kill sendmail */
# define CMDDBGWIZ	14	/* wiz -- become a wizard */
# define CMDONEX	15	/* onex -- sending one transaction only */

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
# ifdef DEBUG
	"showq",	CMDDBGQSHOW,
	"debug",	CMDDBGDEBUG,
	"kill",		CMDDBGKILL,
	"wiz",		CMDDBGWIZ,
# endif DEBUG
	NULL,		CMDERROR,
};

bool	IsWiz = FALSE;			/* set if we are a wizard */
char	*WizWord;			/* the wizard word to compare against */
bool	InChild = FALSE;		/* true if running in a subprocess */
bool	OneXact = FALSE;		/* one xaction only this run */
char	*RealHostName = NULL;		/* verified hostname, set in daemon.c */

#define EX_QUIT		22		/* special code for QUIT command */

smtp()
{
	register char *p;
	register struct cmd *c;
	char *cmd;
	extern char *skipword();
	extern bool sameword();
	bool hasmail;			/* mail command received */
	int rcps;			/* number of recipients */
	extern ADDRESS *sendto();
	ADDRESS *a;

	hasmail = FALSE;
	rcps = 0;
	if (OutChannel != stdout)
	{
		/* arrange for debugging output to go to remote host */
		(void) close(1);
		(void) dup(fileno(OutChannel));
	}
	settime();
	expand("\001e", inp, &inp[sizeof inp], CurEnv);
	message("220", inp);
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
			message("421", "%s Lost input channel", HostName);
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
		while (*++p != '\0' && !isspace(*p))
			continue;
		if (*p != '\0')
			*p++ = '\0';

		/* decode command */
		for (c = CmdTab; c->cmdname != NULL; c++)
		{
			if (sameword(c->cmdname, cmd))
				break;
		}

		/* process command */
		switch (c->cmdcode)
		{
		  case CMDHELO:		/* hello -- introduce yourself */
			if (sameword(p, HostName))
			{
				/* connected to an echo server */
				message("553", "%s I refuse to talk to myself",
					HostName);
				break;
			}
			if (RealHostName != NULL && !sameword(p, RealHostName))
			{
				char buf[MAXNAME];

				(void) sprintf(buf, "%s (%s)", p, RealHostName);
				define('s', newstr(buf), CurEnv);
			}
			else
				define('s', newstr(p), CurEnv);
			message("250", "%s Hello %s, pleased to meet you", HostName, p);
			break;

		  case CMDMAIL:		/* mail -- designate sender */
			/* force a sending host even if no HELO given */
			if (RealHostName != NULL && macvalue('s', CurEnv) == NULL)
				define('s', RealHostName, CurEnv);

			/* check for validity of this command */
			if (hasmail)
			{
				message("503", "Sender already specified");
				break;
			}
			if (InChild)
			{
				syserr("Nested MAIL command");
				exit(0);
			}

			/* fork a subprocess to process this command */
			if (runinchild("SMTP-MAIL") > 0)
				break;
			initsys();

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
			if (setjmp(TopFrame) > 0)
			{
				CurEnv->e_flags &= ~EF_FATALERRS;
				break;
			}
			QuickAbort = TRUE;
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
			CurEnv->e_to = p;
			if (!bitset(QBADADDR, a->q_flags))
				message("250", "Recipient ok");
			else
			{
				/* punt -- should keep message in ADDRESS.... */
				message("550", "Addressee unknown");
			}
			CurEnv->e_to = NULL;
			rcps++;
			break;

		  case CMDDATA:		/* data -- text of mail */
			if (!hasmail)
			{
				message("503", "Need MAIL command");
				break;
			}
			else if (rcps <= 0)
			{
				message("503", "Need RCPT (recipient)");
				break;
			}

			/* collect the text of the message */
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

			if (rcps != 1)
			{
				HoldErrs = TRUE;
				ErrorMode = EM_MAIL;
			}
			CurEnv->e_flags &= ~EF_FATALERRS;
			CurEnv->e_xfp = freopen(queuename(CurEnv, 'x'), "w", CurEnv->e_xfp);

			/* send to all recipients */
			sendall(CurEnv, SM_DEFAULT);
			CurEnv->e_to = NULL;

			/* issue success if appropriate and reset */
			if (Errors == 0 || HoldErrs)
				message("250", "Ok");
			else
				CurEnv->e_flags &= ~EF_FATALERRS;

			/* if in a child, pop back to our parent */
			if (InChild)
				finis();
			break;

		  case CMDRSET:		/* rset -- reset state */
			message("250", "Reset state");
			if (InChild)
				finis();
			break;

		  case CMDVRFY:		/* vrfy -- verify address */
			if (runinchild("SMTP-VRFY") > 0)
				break;
				paddrtree(a);
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
			message("221", "%s closing connection", HostName);
			if (InChild)
				ExitStat = EX_QUIT;
			finis();

		  case CMDVERB:		/* set verbose mode */
			Verbose = TRUE;
			message("200", "Verbose mode");
			break;

		  case CMDONEX:		/* doing one transaction only */
			OneXact = TRUE;
			message("200", "Only one transaction");
			break;

# ifdef DEBUG
		  case CMDDBGQSHOW:	/* show queues */
			printf("Send Queue=");
			printaddr(CurEnv->e_sendqueue, TRUE);
			break;

		  case CMDDBGDEBUG:	/* set debug mode */
			tTsetup(tTdvect, sizeof tTdvect, "0-99.1");
			tTflag(p);
			message("200", "Debug set");
			break;

		  case CMDDBGKILL:	/* kill the parent */
			if (!iswiz())
				break;
			if (kill(MotherPid, SIGTERM) >= 0)
				message("200", "Mother is dead");
			else
				message("500", "Can't kill Mom");
			break;

		  case CMDDBGWIZ:	/* become a wizard */
			if (WizWord != NULL)
			{
				char seed[3];
				extern char *crypt();

				(void) strncpy(seed, WizWord, 2);
				if (strcmp(WizWord, crypt(p, seed)) == 0)
				{
					IsWiz = TRUE;
					message("200", "Please pass, oh mighty wizard");
					break;
				}
			}
			message("500", "You are no wizard!");
			break;
# endif DEBUG

		  case CMDERROR:	/* unknown command */
			message("500", "Command unrecognized");
			break;

		  default:
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
	extern bool sameword();

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
	if (!sameword(q, w))
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
**  ISWIZ -- tell us if we are a wizard
**
**	If not, print a nasty message.
**
**	Parameters:
**		none.
**
**	Returns:
**		TRUE if we are a wizard.
**		FALSE if we are not a wizard.
**
**	Side Effects:
**		Prints a 500 exit stat if we are not a wizard.
*/

#ifdef DEBUG

bool
iswiz()
{
	if (!IsWiz)
		message("500", "Mere mortals musn't mutter that mantra");
	return (IsWiz);
}

#endif DEBUG
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
		}
	}

	/* child (or ONEX command specified) */
	clearenvelope(CurEnv);

	/* open alias database */
	initaliases(AliasFile, FALSE);

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

# endif SMTP
