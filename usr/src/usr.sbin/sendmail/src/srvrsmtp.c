# include "sendmail.h"

static char	SccsId[] =	"@(#)srvrsmtp.c	3.6	%G%";

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
# define CMDMRCP	2	/* mrcp -- designate recipient */
# define CMDDATA	3	/* data -- send message text */
# define CMDDOIT	4	/* doit -- actually do delivery */
# define CMDRSET	5	/* rset -- reset state */
# define CMDVRFY	6	/* vrfy -- verify address */
# define CMDHELP	7	/* help -- give usage info */
# define CMDNOOP	8	/* noop -- do nothing */
# define CMDQUIT	9	/* quit -- close connection and die */
# define CMDMRSQ	10	/* mrsq -- for old mtp compat only */

static struct cmd	CmdTab[] =
{
	"mail",		CMDMAIL,
	"mrcp",		CMDMRCP,
	"data",		CMDDATA,
	"doit",		CMDDOIT,
	"rset",		CMDRSET,
	"vrfy",		CMDVRFY,
	"help",		CMDHELP,
	"noop",		CMDNOOP,
	"quit",		CMDQUIT,
	"mrsq",		CMDMRSQ,
	NULL,		CMDERROR,
};

smtp()
{
	char inp[MAXLINE];
	register char *p;
	struct cmd *c;
	char *cmd;
	extern char *skipword();
	extern bool sameword();
	bool hasmail;			/* mail command received */
	bool hasmrcp;			/* has a recipient */
	bool hasdata;			/* has mail data */

	hasmail = hasmrcp = hasdata = FALSE;
	message("220", "%s Sendmail at your service", HostName);
	for (;;)
	{
		To = NULL;
		Errors = 0;
		if (fgets(inp, sizeof inp, InChannel) == NULL)
		{
			/* end of file, just die */
			message("421", "%s Lost input channel", HostName);
			finis();
		}

		/* clean up end of line */
		fixcrlf(inp, TRUE);

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
		  case CMDMAIL:		/* mail -- designate sender */
			if (hasmail)
			{
				message("503", "Sender already specified");
				break;
			}
			p = skipword(p, "from");
			if (p == NULL)
				break;
			if (index(p, ',') != NULL)
			{
				message("501", "Source routing not implemented");
				Errors++;
				break;
			}
			setsender(p);
			if (Errors == 0)
			{
				message("250", "Sender ok");
				hasmail = TRUE;
			}
			break;

		  case CMDMRCP:		/* mrcp -- designate recipient */
			p = skipword(p, "to");
			if (p == NULL)
				break;
			if (index(p, ',') != NULL)
			{
				message("501", "Source routing not implemented");
				Errors++;
				break;
			}
			sendto(p, 1, (ADDRESS *) NULL);
			if (Errors == 0)
			{
				message("250", "Recipient ok");
				hasmrcp = TRUE;
			}
			break;

		  case CMDDATA:		/* data -- text of mail */
			message("354", "Enter mail, end with dot");
			collect();
			if (Errors == 0)
			{
				message("250", "Message stored");
				hasdata = TRUE;
			}
			break;

		  case CMDDOIT:		/* doit -- actually send everything */
			if (!hasmail)
				message("503", "Need MAIL command");
			else if (!hasmrcp)
				message("503", "Need MRCP (recipient)");
			else if (!hasdata)
				message("503", "No message, use DATA");
			else
			{
				sendall(FALSE);
				if (Errors == 0)
					message("250", "Sent");
			}
			break;

		  case CMDRSET:		/* rset -- reset state */
			message("250", "Reset state");
			finis();

		  case CMDVRFY:		/* vrfy -- verify address */
			sendto(p, 1, (ADDRESS *) NULL);
			if (Errors == 0)
				message("250", "user ok");
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
			finis();

		  case CMDMRSQ:		/* mrsq -- negotiate protocol */
			if (*p == 'R' || *p == 'T')
			{
				/* recipients first or text first */
				message("200", "%c ok, please continue", *p);
			}
			else if (*p == '?')
			{
				/* what do I prefer?  anything, anytime */
				message("215", "R Recipients first is my choice");
			}
			else if (*p == '\0')
			{
				/* no meaningful scheme */
				message("200", "okey dokie boobie");
			}
			else
			{
				/* bad argument */
				message("504", "Scheme unknown");
			}
			break;

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
	extern char *HelpFile;

	hf = fopen(HelpFile, "r");
	if (hf == NULL)
	{
		/* no help */
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
