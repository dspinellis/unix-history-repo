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
static char sccsid[] = "@(#)usersmtp.c	5.13 (Berkeley) %G% (with SMTP)";
#else
static char sccsid[] = "@(#)usersmtp.c	5.13 (Berkeley) %G% (without SMTP)";
#endif
#endif /* not lint */

# include <sysexits.h>
# include <errno.h>

# ifdef SMTP

/*
**  USERSMTP -- run SMTP protocol from the user end.
**
**	This protocol is described in RFC821.
*/

#define REPLYTYPE(r)	((r) / 100)		/* first digit of reply code */
#define REPLYCLASS(r)	(((r) / 10) % 10)	/* second digit of reply code */
#define SMTPCLOSING	421			/* "Service Shutting Down" */

char	SmtpMsgBuffer[MAXLINE];		/* buffer for commands */
char	SmtpReplyBuffer[MAXLINE];	/* buffer for replies */
char	SmtpError[MAXLINE] = "";	/* save failure error messages */
FILE	*SmtpOut;			/* output file */
FILE	*SmtpIn;			/* input file */
int	SmtpPid;			/* pid of mailer */

/* following represents the state of the SMTP connection */
int	SmtpState;			/* connection state, see below */

#define SMTP_CLOSED	0		/* connection is closed */
#define SMTP_OPEN	1		/* connection is open for business */
#define SMTP_SSD	2		/* service shutting down */
/*
**  SMTPINIT -- initialize SMTP.
**
**	Opens the connection and sends the initial protocol.
**
**	Parameters:
**		m -- mailer to create connection to.
**		pvp -- pointer to parameter vector to pass to
**			the mailer.
**
**	Returns:
**		appropriate exit status -- EX_OK on success.
**		If not EX_OK, it should close the connection.
**
**	Side Effects:
**		creates connection and sends initial protocol.
*/

jmp_buf	CtxGreeting;

smtpinit(m, pvp)
	struct mailer *m;
	char **pvp;
{
	register int r;
	EVENT *gte;
	char buf[MAXNAME];
	extern greettimeout();

	/*
	**  Open the connection to the mailer.
	*/

	if (SmtpState == SMTP_OPEN)
		syserr("smtpinit: already open");

	SmtpIn = SmtpOut = NULL;
	SmtpState = SMTP_CLOSED;
	SmtpError[0] = '\0';
	SmtpPhase = "user open";
	setproctitle("%s %s: %s", CurEnv->e_id, pvp[1], SmtpPhase);
	SmtpPid = openmailer(m, pvp, (ADDRESS *) NULL, TRUE, &SmtpOut, &SmtpIn);
	if (SmtpPid < 0)
	{
		if (tTd(18, 1))
			printf("smtpinit: cannot open %s: stat %d errno %d\n",
			   pvp[0], ExitStat, errno);
		if (CurEnv->e_xfp != NULL)
		{
			register char *p;
			extern char *errstring();
			extern char *statstring();

			if (errno == 0)
			{
				p = statstring(ExitStat);
				fprintf(CurEnv->e_xfp,
					"%.3s %s.%s... %s\n",
					p, pvp[1], m->m_name, p);
			}
			else
			{
				r = errno;
				fprintf(CurEnv->e_xfp,
					"421 %s.%s... Deferred: %s\n",
					pvp[1], m->m_name, errstring(errno));
				errno = r;
			}
		}
		return (ExitStat);
	}
	SmtpState = SMTP_OPEN;

	/*
	**  Get the greeting message.
	**	This should appear spontaneously.  Give it five minutes to
	**	happen.
	*/

	if (setjmp(CtxGreeting) != 0)
		goto tempfail;
	gte = setevent((time_t) 300, greettimeout, 0);
	SmtpPhase = "greeting wait";
	setproctitle("%s %s: %s", CurEnv->e_id, CurHostName, SmtpPhase);
	r = reply(m);
	clrevent(gte);
	if (r < 0 || REPLYTYPE(r) != 2)
		goto tempfail;

	/*
	**  Send the HELO command.
	**	My mother taught me to always introduce myself.
	*/

	smtpmessage("HELO %s", m, MyHostName);
	SmtpPhase = "HELO wait";
	setproctitle("%s %s: %s", CurEnv->e_id, CurHostName, SmtpPhase);
	r = reply(m);
	if (r < 0)
		goto tempfail;
	else if (REPLYTYPE(r) == 5)
		goto unavailable;
	else if (REPLYTYPE(r) != 2)
		goto tempfail;

	/*
	**  If this is expected to be another sendmail, send some internal
	**  commands.
	*/

	if (bitnset(M_INTERNAL, m->m_flags))
	{
		/* tell it to be verbose */
		smtpmessage("VERB", m);
		r = reply(m);
		if (r < 0)
			goto tempfail;

		/* tell it we will be sending one transaction only */
		smtpmessage("ONEX", m);
		r = reply(m);
		if (r < 0)
			goto tempfail;
	}

	/*
	**  Send the HOPS command.
	**	This is non-standard and may give an "unknown command".
	**		This is not an error.
	**	It can give a "bad hop count" error if the hop
	**		count is exceeded.
	*/

	/*
	**  Send the MAIL command.
	**	Designates the sender.
	*/

	expand("\001g", buf, &buf[sizeof buf - 1], CurEnv);
	if (CurEnv->e_from.q_mailer == LocalMailer ||
	    !bitnset(M_FROMPATH, m->m_flags))
	{
		smtpmessage("MAIL From:<%s>", m, buf);
	}
	else
	{
		smtpmessage("MAIL From:<@%s%c%s>", m, MyHostName,
			buf[0] == '@' ? ',' : ':', buf);
	}
	SmtpPhase = "MAIL wait";
	setproctitle("%s %s: %s", CurEnv->e_id, CurHostName, SmtpPhase);
	r = reply(m);
	if (r < 0 || REPLYTYPE(r) == 4)
		goto tempfail;
	else if (r == 250)
		return (EX_OK);
	else if (r == 552)
		goto unavailable;

	/* protocol error -- close up */
	smtpquit(m);
	return (EX_PROTOCOL);

	/* signal a temporary failure */
  tempfail:
	smtpquit(m);
	return (EX_TEMPFAIL);

	/* signal service unavailable */
  unavailable:
	smtpquit(m);
	return (EX_UNAVAILABLE);
}


static
greettimeout()
{
	/* timeout reading the greeting message */
	longjmp(CtxGreeting, 1);
}
/*
**  SMTPRCPT -- designate recipient.
**
**	Parameters:
**		to -- address of recipient.
**		m -- the mailer we are sending to.
**
**	Returns:
**		exit status corresponding to recipient status.
**
**	Side Effects:
**		Sends the mail via SMTP.
*/

smtprcpt(to, m)
	ADDRESS *to;
	register MAILER *m;
{
	register int r;
	extern char *remotename();

	smtpmessage("RCPT To:<%s>", m, remotename(to->q_user, m, FALSE, TRUE));

	SmtpPhase = "RCPT wait";
	setproctitle("%s %s: %s", CurEnv->e_id, CurHostName, SmtpPhase);
	r = reply(m);
	if (r < 0 || REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	else if (REPLYTYPE(r) == 2)
		return (EX_OK);
	else if (r == 550 || r == 551 || r == 553)
		return (EX_NOUSER);
	else if (r == 552 || r == 554)
		return (EX_UNAVAILABLE);
	return (EX_PROTOCOL);
}
/*
**  SMTPDATA -- send the data and clean up the transaction.
**
**	Parameters:
**		m -- mailer being sent to.
**		e -- the envelope for this message.
**
**	Returns:
**		exit status corresponding to DATA command.
**
**	Side Effects:
**		none.
*/

smtpdata(m, e)
	struct mailer *m;
	register ENVELOPE *e;
{
	register int r;

	/*
	**  Send the data.
	**	First send the command and check that it is ok.
	**	Then send the data.
	**	Follow it up with a dot to terminate.
	**	Finally get the results of the transaction.
	*/

	/* send the command and check ok to proceed */
	smtpmessage("DATA", m);
	SmtpPhase = "DATA wait";
	setproctitle("%s %s: %s", CurEnv->e_id, CurHostName, SmtpPhase);
	r = reply(m);
	if (r < 0 || REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	else if (r == 554)
		return (EX_UNAVAILABLE);
	else if (r != 354)
		return (EX_PROTOCOL);

	/* now output the actual message */
	(*e->e_puthdr)(SmtpOut, m, CurEnv);
	putline("\n", SmtpOut, m);
	(*e->e_putbody)(SmtpOut, m, CurEnv);

	/* terminate the message */
	fprintf(SmtpOut, ".%s", m->m_eol);
	if (Verbose && !HoldErrs)
		nmessage(Arpa_Info, ">>> .");

	/* check for the results of the transaction */
	SmtpPhase = "result wait";
	setproctitle("%s %s: %s", CurEnv->e_id, CurHostName, SmtpPhase);
	r = reply(m);
	if (r < 0 || REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	else if (r == 250)
		return (EX_OK);
	else if (r == 552 || r == 554)
		return (EX_UNAVAILABLE);
	return (EX_PROTOCOL);
}
/*
**  SMTPQUIT -- close the SMTP connection.
**
**	Parameters:
**		m -- a pointer to the mailer.
**
**	Returns:
**		none.
**
**	Side Effects:
**		sends the final protocol and closes the connection.
*/

smtpquit(m)
	register MAILER *m;
{
	int i;

	/* if the connection is already closed, don't bother */
	if (SmtpIn == NULL)
		return;

	/* send the quit message if not a forced quit */
	if (SmtpState == SMTP_OPEN || SmtpState == SMTP_SSD)
	{
		smtpmessage("QUIT", m);
		(void) reply(m);
		if (SmtpState == SMTP_CLOSED)
			return;
	}

	/* now actually close the connection */
	(void) fclose(SmtpIn);
	(void) fclose(SmtpOut);
	SmtpIn = SmtpOut = NULL;
	SmtpState = SMTP_CLOSED;

	/* and pick up the zombie */
	i = endmailer(SmtpPid, m->m_argv[0]);
	if (i != EX_OK)
		syserr("smtpquit %s: stat %d", m->m_argv[0], i);
}
/*
**  REPLY -- read arpanet reply
**
**	Parameters:
**		m -- the mailer we are reading the reply from.
**
**	Returns:
**		reply code it reads.
**
**	Side Effects:
**		flushes the mail file.
*/

reply(m)
	MAILER *m;
{
	(void) fflush(SmtpOut);

	if (tTd(18, 1))
		printf("reply\n");

	/*
	**  Read the input line, being careful not to hang.
	*/

	for (;;)
	{
		register int r;
		register char *p;

		/* actually do the read */
		if (CurEnv->e_xfp != NULL)
			(void) fflush(CurEnv->e_xfp);	/* for debugging */

		/* if we are in the process of closing just give the code */
		if (SmtpState == SMTP_CLOSED)
			return (SMTPCLOSING);

		/* get the line from the other side */
		p = sfgets(SmtpReplyBuffer, sizeof SmtpReplyBuffer, SmtpIn);
		if (p == NULL)
		{
			extern char MsgBuf[];		/* err.c */
			extern char Arpa_TSyserr[];	/* conf.c */

			/* if the remote end closed early, fake an error */
			if (errno == 0)
# ifdef ECONNRESET
				errno = ECONNRESET;
# else ECONNRESET
				errno = EPIPE;
# endif ECONNRESET

			message(Arpa_TSyserr, "reply: read error");
			/* if debugging, pause so we can see state */
			if (tTd(18, 100))
				pause();
# ifdef LOG
			syslog(LOG_INFO, "%s", &MsgBuf[4]);
# endif LOG
			SmtpState = SMTP_CLOSED;
			smtpquit(m);
			return (-1);
		}
		fixcrlf(SmtpReplyBuffer, TRUE);

		if (CurEnv->e_xfp != NULL && index("45", SmtpReplyBuffer[0]) != NULL)
		{
			/* serious error -- log the previous command */
			if (SmtpMsgBuffer[0] != '\0')
				fprintf(CurEnv->e_xfp, ">>> %s\n", SmtpMsgBuffer);
			SmtpMsgBuffer[0] = '\0';

			/* now log the message as from the other side */
			fprintf(CurEnv->e_xfp, "<<< %s\n", SmtpReplyBuffer);
		}

		/* display the input for verbose mode */
		if (Verbose && !HoldErrs)
			nmessage(Arpa_Info, "%s", SmtpReplyBuffer);

		/* if continuation is required, we can go on */
		if (SmtpReplyBuffer[3] == '-' || !isdigit(SmtpReplyBuffer[0]))
			continue;

		/* decode the reply code */
		r = atoi(SmtpReplyBuffer);

		/* extra semantics: 0xx codes are "informational" */
		if (r < 100)
			continue;

		/* reply code 421 is "Service Shutting Down" */
		if (r == SMTPCLOSING && SmtpState != SMTP_SSD)
		{
			/* send the quit protocol */
			SmtpState = SMTP_SSD;
			smtpquit(m);
		}

		/* save temporary failure messages for posterity */
		if (SmtpReplyBuffer[0] == '4' && SmtpError[0] == '\0')
			(void) strcpy(SmtpError, &SmtpReplyBuffer[4]);

		return (r);
	}
}
/*
**  SMTPMESSAGE -- send message to server
**
**	Parameters:
**		f -- format
**		m -- the mailer to control formatting.
**		a, b, c -- parameters
**
**	Returns:
**		none.
**
**	Side Effects:
**		writes message to SmtpOut.
*/

/*VARARGS1*/
smtpmessage(f, m, a, b, c)
	char *f;
	MAILER *m;
{
	(void) sprintf(SmtpMsgBuffer, f, a, b, c);
	if (tTd(18, 1) || (Verbose && !HoldErrs))
		nmessage(Arpa_Info, ">>> %s", SmtpMsgBuffer);
	if (SmtpOut != NULL)
		fprintf(SmtpOut, "%s%s", SmtpMsgBuffer, m->m_eol);
}

# endif SMTP
