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
static char sccsid[] = "@(#)usersmtp.c	6.33 (Berkeley) %G% (with SMTP)";
#else
static char sccsid[] = "@(#)usersmtp.c	6.33 (Berkeley) %G% (without SMTP)";
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
int	SmtpPid;			/* pid of mailer */

#ifdef __STDC__
extern	smtpmessage(char *f, MAILER *m, MCI *mci, ...);
#endif
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
**		none.
**
**	Side Effects:
**		creates connection and sends initial protocol.
*/

smtpinit(m, mci, e)
	struct mailer *m;
	register MCI *mci;
	ENVELOPE *e;
{
	register int r;
	register char *p;
	extern void esmtp_check();
	extern void helo_options();

	if (tTd(17, 1))
	{
		printf("smtpinit ");
		mci_dump(mci);
	}

	/*
	**  Open the connection to the mailer.
	*/

	SmtpError[0] = '\0';
	CurHostName = mci->mci_host;		/* XXX UGLY XXX */
	switch (mci->mci_state)
	{
	  case MCIS_ACTIVE:
		/* need to clear old information */
		smtprset(m, mci, e);
		/* fall through */

	  case MCIS_OPEN:
		return;

	  case MCIS_ERROR:
	  case MCIS_SSD:
		/* shouldn't happen */
		smtpquit(m, mci, e);
		/* fall through */

	  case MCIS_CLOSED:
		syserr("451 smtpinit: state CLOSED");
		return;

	  case MCIS_OPENING:
		break;
	}

	mci->mci_state = MCIS_OPENING;

	/*
	**  Get the greeting message.
	**	This should appear spontaneously.  Give it five minutes to
	**	happen.
	*/

	SmtpPhase = mci->mci_phase = "client greeting";
	setproctitle("%s %s: %s", e->e_id, CurHostName, mci->mci_phase);
	r = reply(m, mci, e, TimeOuts.to_initial, esmtp_check);
	if (r < 0 || REPLYTYPE(r) != 2)
		goto tempfail1;

	/*
	**  Send the HELO command.
	**	My mother taught me to always introduce myself.
	*/

	if (bitnset(M_ESMTP, m->m_flags))
		mci->mci_flags |= MCIF_ESMTP;

tryhelo:
	if (bitset(MCIF_ESMTP, mci->mci_flags))
	{
		smtpmessage("EHLO %s", m, mci, MyHostName);
		SmtpPhase = mci->mci_phase = "client EHLO";
	}
	else
	{
		smtpmessage("HELO %s", m, mci, MyHostName);
		SmtpPhase = mci->mci_phase = "client HELO";
	}
	setproctitle("%s %s: %s", e->e_id, CurHostName, mci->mci_phase);
	r = reply(m, mci, e, TimeOuts.to_helo, helo_options);
	if (r < 0)
		goto tempfail1;
	else if (REPLYTYPE(r) == 5)
	{
		if (bitset(MCIF_ESMTP, mci->mci_flags))
		{
			/* try old SMTP instead */
			mci->mci_flags &= ~MCIF_ESMTP;
			goto tryhelo;
		}
		goto unavailable;
	}
	else if (REPLYTYPE(r) != 2)
		goto tempfail1;

	/*
	**  Check to see if we actually ended up talking to ourself.
	**  This means we didn't know about an alias or MX, or we managed
	**  to connect to an echo server.
	*/

	p = strchr(&SmtpReplyBuffer[4], ' ');
	if (p != NULL)
		*p == '\0';
	if (strcasecmp(&SmtpReplyBuffer[4], MyHostName) == 0)
	{
		syserr("553 %s config error: mail loops back to myself",
			MyHostName);
		mci->mci_exitstat = EX_CONFIG;
		mci->mci_errno = 0;
		smtpquit(m, mci, e);
		return;
	}

	/*
	**  If this is expected to be another sendmail, send some internal
	**  commands.
	*/

	if (bitnset(M_INTERNAL, m->m_flags))
	{
		/* tell it to be verbose */
		smtpmessage("VERB", m, mci);
		r = reply(m, mci, e, TimeOuts.to_miscshort, NULL);
		if (r < 0)
			goto tempfail2;
	}

	mci->mci_state = MCIS_OPEN;
	return;

  tempfail1:
  tempfail2:
	mci->mci_exitstat = EX_TEMPFAIL;
	if (mci->mci_errno == 0)
		mci->mci_errno = errno;
	if (mci->mci_state != MCIS_CLOSED)
		smtpquit(m, mci, e);
	return;

  unavailable:
	mci->mci_exitstat = EX_UNAVAILABLE;
	mci->mci_errno = errno;
	smtpquit(m, mci, e);
	return;
}
/*
**  ESMTP_CHECK -- check to see if this implementation likes ESMTP protocol
**
**
**	Parameters:
**		line -- the response line.
**		m -- the mailer.
**		mci -- the mailer connection info.
**		e -- the envelope.
**
**	Returns:
**		none.
*/

void
esmtp_check(line, m, mci, e)
	char *line;
	MAILER *m;
	register MCI *mci;
	ENVELOPE *e;
{
	if (strlen(line) < 5)
		return;
	line += 4;
	if (strncmp(line, "ESMTP ", 6) == 0)
		mci->mci_flags |= MCIF_ESMTP;
}
/*
**  HELO_OPTIONS -- process the options on a HELO line.
**
**	Parameters:
**		line -- the response line.
**		m -- the mailer.
**		mci -- the mailer connection info.
**		e -- the envelope.
**
**	Returns:
**		none.
*/

void
helo_options(line, m, mci, e)
	char *line;
	MAILER *m;
	register MCI *mci;
	ENVELOPE *e;
{
	register char *p;

	if (strlen(line) < 5)
		return;
	line += 4;
	p = strchr(line, ' ');
	if (p != NULL)
		*p++ = '\0';
	if (strcasecmp(line, "size") == 0)
	{
		mci->mci_flags |= MCIF_SIZE;
		if (p != NULL)
			mci->mci_maxsize = atol(p);
	}
	else if (strcasecmp(line, "8bitmime") == 0)
		mci->mci_flags |= MCIF_8BITMIME;
	else if (strcasecmp(line, "expn") == 0)
		mci->mci_flags |= MCIF_EXPN;
}
/*
**  SMTPMAILFROM -- send MAIL command
**
**	Parameters:
**		m -- the mailer.
**		mci -- the mailer connection structure.
**		e -- the envelope (including the sender to specify).
*/

smtpmailfrom(m, mci, e)
	struct mailer *m;
	MCI *mci;
	ENVELOPE *e;
{
	int r;
	char buf[MAXNAME];
	char optbuf[MAXLINE];

	if (tTd(17, 2))
		printf("smtpmailfrom: CurHost=%s\n", CurHostName);

	/* set up appropriate options to include */
	if (bitset(MCIF_SIZE, mci->mci_flags))
		sprintf(optbuf, " SIZE=%ld", e->e_msgsize);
	else
		strcpy(optbuf, "");

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

	mci->mci_state = MCIS_ACTIVE;

	if (bitset(EF_RESPONSE, e->e_flags) &&
	    !bitnset(M_NO_NULL_FROM, m->m_flags))
		(void) strcpy(buf, "");
	else
		expand("\201g", buf, &buf[sizeof buf - 1], e);
	if (e->e_from.q_mailer == LocalMailer ||
	    !bitnset(M_FROMPATH, m->m_flags))
	{
		smtpmessage("MAIL From:<%s>%s", m, mci, buf, optbuf);
	}
	else
	{
		smtpmessage("MAIL From:<@%s%c%s>%s", m, mci, MyHostName,
			buf[0] == '@' ? ',' : ':', buf, optbuf);
	}
	SmtpPhase = mci->mci_phase = "client MAIL";
	setproctitle("%s %s: %s", e->e_id, CurHostName, mci->mci_phase);
	r = reply(m, mci, e, TimeOuts.to_mail, NULL);
	if (r < 0 || REPLYTYPE(r) == 4)
	{
		mci->mci_exitstat = EX_TEMPFAIL;
		mci->mci_errno = errno;
		smtpquit(m, mci, e);
		return EX_TEMPFAIL;
	}
	else if (r == 250)
	{
		mci->mci_exitstat = EX_OK;
		return EX_OK;
	}
	else if (r == 552)
	{
		/* signal service unavailable */
		mci->mci_exitstat = EX_UNAVAILABLE;
		smtpquit(m, mci, e);
		return EX_UNAVAILABLE;
	}

#ifdef LOG
	if (LogLevel > 1)
	{
		syslog(LOG_CRIT, "%s: SMTP MAIL protocol error: %s",
			e->e_id, SmtpReplyBuffer);
	}
#endif

	/* protocol error -- close up */
	smtpquit(m, mci, e);
	mci->mci_exitstat = EX_PROTOCOL;
	return EX_PROTOCOL;
}
/*
**  SMTPRCPT -- designate recipient.
**
**	Parameters:
**		to -- address of recipient.
**		m -- the mailer we are sending to.
**		mci -- the connection info for this transaction.
**		e -- the envelope for this transaction.
**
**	Returns:
**		exit status corresponding to recipient status.
**
**	Side Effects:
**		Sends the mail via SMTP.
*/

smtprcpt(to, m, mci, e)
	ADDRESS *to;
	register MAILER *m;
	MCI *mci;
	ENVELOPE *e;
{
	register int r;

	smtpmessage("RCPT To:<%s>", m, mci, to->q_user);

	SmtpPhase = mci->mci_phase = "client RCPT";
	setproctitle("%s %s: %s", e->e_id, CurHostName, mci->mci_phase);
	r = reply(m, mci, e, TimeOuts.to_rcpt, NULL);
	if (r < 0 || REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	else if (REPLYTYPE(r) == 2)
		return (EX_OK);
	else if (r == 550 || r == 551 || r == 553)
		return (EX_NOUSER);
	else if (r == 552 || r == 554)
		return (EX_UNAVAILABLE);

#ifdef LOG
	if (LogLevel > 1)
	{
		syslog(LOG_CRIT, "%s: SMTP RCPT protocol error: %s",
			e->e_id, SmtpReplyBuffer);
	}
#endif

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

smtpdata(m, mci, e)
	struct mailer *m;
	register MCI *mci;
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
	smtpmessage("DATA", m, mci);
	SmtpPhase = mci->mci_phase = "client DATA 354";
	setproctitle("%s %s: %s", e->e_id, CurHostName, mci->mci_phase);
	r = reply(m, mci, e, TimeOuts.to_datainit, NULL);
	if (r < 0 || REPLYTYPE(r) == 4)
	{
		smtpquit(m, mci, e);
		return (EX_TEMPFAIL);
	}
	else if (r == 554)
	{
		smtprset(m, mci, e);
		return (EX_UNAVAILABLE);
	}
	else if (r != 354)
	{
#ifdef LOG
		if (LogLevel > 1)
		{
			syslog(LOG_CRIT, "%s: SMTP DATA-1 protocol error: %s",
				e->e_id, SmtpReplyBuffer);
		}
#endif
		smtprset(m, mci, e);
		return (EX_PROTOCOL);
	}

	/* now output the actual message */
	(*e->e_puthdr)(mci->mci_out, m, e);
	putline("\n", mci->mci_out, m);
	(*e->e_putbody)(mci->mci_out, m, e, NULL);

	/* terminate the message */
	fprintf(mci->mci_out, ".%s", m->m_eol);
	if (Verbose)
		nmessage(">>> .");

	/* check for the results of the transaction */
	SmtpPhase = mci->mci_phase = "client DATA 250";
	setproctitle("%s %s: %s", e->e_id, CurHostName, mci->mci_phase);
	r = reply(m, mci, e, TimeOuts.to_datafinal, NULL);
	if (r < 0)
	{
		smtpquit(m, mci, e);
		return (EX_TEMPFAIL);
	}
	mci->mci_state = MCIS_OPEN;
	e->e_statmsg = newstr(&SmtpReplyBuffer[4]);
	if (REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	else if (r == 250)
		return (EX_OK);
	else if (r == 552 || r == 554)
		return (EX_UNAVAILABLE);
#ifdef LOG
	if (LogLevel > 1)
	{
		syslog(LOG_CRIT, "%s: SMTP DATA-2 protocol error: %s",
			e->e_id, SmtpReplyBuffer);
	}
#endif
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

smtpquit(m, mci, e)
	register MAILER *m;
	register MCI *mci;
	ENVELOPE *e;
{
	int i;

	/* send the quit message if we haven't gotten I/O error */
	if (mci->mci_state != MCIS_ERROR)
	{
		SmtpPhase = "client QUIT";
		smtpmessage("QUIT", m, mci);
		(void) reply(m, mci, e, TimeOuts.to_quit, NULL);
		if (mci->mci_state == MCIS_CLOSED)
			return;
	}

	/* now actually close the connection and pick up the zombie */
	i = endmailer(mci, e, m->m_argv);
	if (i != EX_OK)
		syserr("451 smtpquit %s: stat %d", m->m_argv[0], i);
}
/*
**  SMTPRSET -- send a RSET (reset) command
*/

smtprset(m, mci, e)
	register MAILER *m;
	register MCI *mci;
	ENVELOPE *e;
{
	int r;

	SmtpPhase = "client RSET";
	smtpmessage("RSET", m, mci);
	r = reply(m, mci, e, TimeOuts.to_rset, NULL);
	if (r < 0)
		mci->mci_state = MCIS_ERROR;
	else if (REPLYTYPE(r) == 2)
	{
		mci->mci_state = MCIS_OPEN;
		return;
	}
	smtpquit(m, mci, e);
}
/*
**  SMTPPROBE -- check the connection state
*/

smtpprobe(mci)
	register MCI *mci;
{
	int r;
	MAILER *m = mci->mci_mailer;
	extern ENVELOPE BlankEnvelope;
	ENVELOPE *e = &BlankEnvelope;

	SmtpPhase = "client probe";
	smtpmessage("RSET", m, mci);
	r = reply(m, mci, e, TimeOuts.to_miscshort, NULL);
	if (r < 0 || REPLYTYPE(r) != 2)
		smtpquit(m, mci, e);
	return r;
}
/*
**  REPLY -- read arpanet reply
**
**	Parameters:
**		m -- the mailer we are reading the reply from.
**		mci -- the mailer connection info structure.
**		e -- the current envelope.
**		timeout -- the timeout for reads.
**		pfunc -- processing function for second and subsequent
**			lines of response -- if null, no special
**			processing is done.
**
**	Returns:
**		reply code it reads.
**
**	Side Effects:
**		flushes the mail file.
*/

reply(m, mci, e, timeout, pfunc)
	MAILER *m;
	MCI *mci;
	ENVELOPE *e;
	time_t timeout;
	void (*pfunc)();
{
	register char *bufp;
	register int r;
	bool firstline = TRUE;
	char junkbuf[MAXLINE];

	if (mci->mci_out != NULL)
		(void) fflush(mci->mci_out);

	if (tTd(18, 1))
		printf("reply\n");

	/*
	**  Read the input line, being careful not to hang.
	*/

	for (bufp = SmtpReplyBuffer;; bufp = junkbuf)
	{
		register char *p;
		extern time_t curtime();

		/* actually do the read */
		if (e->e_xfp != NULL)
			(void) fflush(e->e_xfp);	/* for debugging */

		/* if we are in the process of closing just give the code */
		if (mci->mci_state == MCIS_CLOSED)
			return (SMTPCLOSING);

		if (mci->mci_out != NULL)
			fflush(mci->mci_out);

		/* get the line from the other side */
		p = sfgets(bufp, MAXLINE, mci->mci_in, timeout, SmtpPhase);
		mci->mci_lastuse = curtime();

		if (p == NULL)
		{
			extern char MsgBuf[];		/* err.c */

			/* if the remote end closed early, fake an error */
			if (errno == 0)
# ifdef ECONNRESET
				errno = ECONNRESET;
# else /* ECONNRESET */
				errno = EPIPE;
# endif /* ECONNRESET */

			mci->mci_errno = errno;
			mci->mci_exitstat = EX_TEMPFAIL;
			message("451 %s: reply: read error from %s",
				e->e_id == NULL ? "NOQUEUE" : e->e_id,
				mci->mci_host);
			/* if debugging, pause so we can see state */
			if (tTd(18, 100))
				pause();
# ifdef LOG
			if (LogLevel > 1)
				syslog(LOG_INFO, "%s", &MsgBuf[4]);
# endif /* LOG */
			mci->mci_state = MCIS_ERROR;
			smtpquit(m, mci, e);
			return (-1);
		}
		fixcrlf(bufp, TRUE);

		if (e->e_xfp != NULL && strchr("45", bufp[0]) != NULL)
		{
			/* serious error -- log the previous command */
			if (SmtpMsgBuffer[0] != '\0')
				fprintf(e->e_xfp, ">>> %s\n", SmtpMsgBuffer);
			SmtpMsgBuffer[0] = '\0';

			/* now log the message as from the other side */
			fprintf(e->e_xfp, "<<< %s\n", bufp);
		}

		/* display the input for verbose mode */
		if (Verbose)
			nmessage("050 %s", bufp);

		/* process the line */
		if (pfunc != NULL && !firstline)
			(*pfunc)(bufp, m, mci, e);

		firstline = FALSE;

		/* if continuation is required, we can go on */
		if (bufp[3] == '-')
			continue;

		/* ignore improperly formated input */
		if (!(isascii(bufp[0]) && isdigit(bufp[0])))
			continue;

		/* decode the reply code */
		r = atoi(bufp);

		/* extra semantics: 0xx codes are "informational" */
		if (r >= 100)
			break;
	}

	/*
	**  Now look at SmtpReplyBuffer -- only care about the first
	**  line of the response from here on out.
	*/

	/* save temporary failure messages for posterity */
	if (SmtpReplyBuffer[0] == '4' && SmtpError[0] == '\0')
		(void) strcpy(SmtpError, SmtpReplyBuffer);

	/* reply code 421 is "Service Shutting Down" */
	if (r == SMTPCLOSING && mci->mci_state != MCIS_SSD)
	{
		/* send the quit protocol */
		mci->mci_state = MCIS_SSD;
		smtpquit(m, mci, e);
	}

	return (r);
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
**		writes message to mci->mci_out.
*/

/*VARARGS1*/
#ifdef __STDC__
smtpmessage(char *f, MAILER *m, MCI *mci, ...)
#else
smtpmessage(f, m, mci, va_alist)
	char *f;
	MAILER *m;
	MCI *mci;
	va_dcl
#endif
{
	VA_LOCAL_DECL

	VA_START(mci);
	(void) vsprintf(SmtpMsgBuffer, f, ap);
	VA_END;

	if (tTd(18, 1) || Verbose)
		nmessage(">>> %s", SmtpMsgBuffer);
	if (mci->mci_out != NULL)
	{
		fprintf(mci->mci_out, "%s%s", SmtpMsgBuffer,
			m == NULL ? "\r\n" : m->m_eol);
	}
	else if (tTd(18, 1))
	{
		printf("smtpmessage: NULL mci_out\n");
	}
}

# endif /* SMTP */
