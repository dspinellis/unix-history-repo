# include <ctype.h>
# include <sysexits.h>
# include "sendmail.h"

# ifndef SMTP
SCCSID(@(#)usersmtp.c	3.39		%G%	(no SMTP));
# else SMTP

SCCSID(@(#)usersmtp.c	3.39		%G%);



/*
**  USERSMTP -- run SMTP protocol from the user end.
**
**	This protocol is described in RFC821.
*/

#define REPLYTYPE(r)	((r) / 100)		/* first digit of reply code */
#define REPLYCLASS(r)	(((r) / 10) % 10)	/* second digit of reply code */
#define SMTPCLOSING	421			/* "Service Shutting Down" */

char	SmtpReplyBuffer[MAXLINE];	/* buffer for replies */
FILE	*SmtpOut;			/* output file */
FILE	*SmtpIn;			/* input file */
int	SmtpPid;			/* pid of mailer */
bool	SmtpClosing;			/* set on a forced close */
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
**
**	Side Effects:
**		creates connection and sends initial protocol.
*/

smtpinit(m, pvp)
	struct mailer *m;
	char **pvp;
{
	register int r;
	char buf[MAXNAME];

	/*
	**  Open the connection to the mailer.
	*/

	SmtpIn = SmtpOut = NULL;
	SmtpClosing = FALSE;
	SmtpPid = openmailer(m, pvp, (ADDRESS *) NULL, TRUE, &SmtpOut, &SmtpIn);
	if (SmtpPid < 0)
	{
# ifdef DEBUG
		if (tTd(18, 1))
			printf("smtpinit: cannot open %s: stat %d errno %d\n",
			   pvp[0], ExitStat, errno);
# endif DEBUG
		return (ExitStat);
	}

	/*
	**  Get the greeting message.
	**	This should appear spontaneously.
	*/

	r = reply(m);
	if (r < 0 || REPLYTYPE(r) != 2)
		return (EX_TEMPFAIL);

	/*
	**  Send the HELO command.
	**	My mother taught me to always introduce myself.
	*/

	smtpmessage("HELO %s", m, HostName);
	r = reply(m);
	if (r < 0)
		return (EX_TEMPFAIL);
	else if (REPLYTYPE(r) == 5)
		return (EX_UNAVAILABLE);
	else if (REPLYTYPE(r) != 2)
		return (EX_TEMPFAIL);

	/*
	**  If this is expected to be another sendmail, send some internal
	**  commands.
	*/

	if (bitset(M_INTERNAL, m->m_flags))
	{
		/* tell it to be verbose */
		smtpmessage("VERB", m);
		r = reply(m);
		if (r < 0)
			return (EX_TEMPFAIL);

		/* tell it we will be sending one transaction only */
		smtpmessage("ONEX", m);
		r = reply(m);
		if (r < 0)
			return (EX_TEMPFAIL);
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

	expand("$g", buf, &buf[sizeof buf - 1], CurEnv);
	if (CurEnv->e_from.q_mailer == LocalMailer ||
	    !bitset(M_FROMPATH, m->m_flags))
	{
		smtpmessage("MAIL From:<%s>", m, buf);
	}
	else
	{
		smtpmessage("MAIL From:<@%s%c%s>", m, HostName,
			buf[0] == '@' ? ',' : ':', buf);
	}
	r = reply(m);
	if (r < 0 || REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	else if (r == 250)
		return (EX_OK);
	else if (r == 552)
		return (EX_UNAVAILABLE);
	return (EX_PROTOCOL);
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
**		name -- name of mailer we are quitting.
**
**	Returns:
**		none.
**
**	Side Effects:
**		sends the final protocol and closes the connection.
*/

smtpquit(name, m)
	char *name;
	register MAILER *m;
{
	int i;

	/* if the connection is already closed, don't bother */
	if (SmtpIn == NULL)
		return;

	/* send the quit message if not a forced quit */
	if (!SmtpClosing)
	{
		smtpmessage("QUIT", m);
		(void) reply(m);
		if (SmtpClosing)
			return;
	}

	/* now actually close the connection */
	(void) fclose(SmtpIn);
	(void) fclose(SmtpOut);
	SmtpIn = SmtpOut = NULL;

	/* and pick up the zombie */
	i = endmailer(SmtpPid, name);
	if (i != EX_OK)
		syserr("smtpquit %s: stat %d", name, i);
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
		if (SmtpClosing)
			return (SMTPCLOSING);

		/* get the line from the other side */
		p = sfgets(SmtpReplyBuffer, sizeof SmtpReplyBuffer, SmtpIn);
		if (p == NULL)
		{
			extern char MsgBuf[];		/* err.c */
			extern char Arpa_TSyserr[];	/* conf.c */

			message(Arpa_TSyserr, "reply: read error");
# ifdef LOG
			syslog(LOG_ERR, "%s", &MsgBuf[4]);
# endif LOG
			SmtpClosing = TRUE;
			smtpquit("reply error", m);
			return (-1);
		}
		fixcrlf(SmtpReplyBuffer, TRUE);

		/* log the input in the transcript for future error returns */
		if (Verbose && !HoldErrs)
			nmessage(Arpa_Info, "%s", SmtpReplyBuffer);
		else if (CurEnv->e_xfp != NULL)
			fprintf(CurEnv->e_xfp, "%s\n", SmtpReplyBuffer);

		/* if continuation is required, we can go on */
		if (SmtpReplyBuffer[3] == '-' || !isdigit(SmtpReplyBuffer[0]))
			continue;

		/* decode the reply code */
		r = atoi(SmtpReplyBuffer);

		/* extra semantics: 0xx codes are "informational" */
		if (r < 100)
			continue;

		/* reply code 421 is "Service Shutting Down" */
		if (r == SMTPCLOSING)
		{
			/* send the quit protocol */
			smtpquit("SMTP Shutdown", m);
			SmtpClosing = TRUE;
		}

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
	char buf[100];

	(void) sprintf(buf, f, a, b, c);
	if (tTd(18, 1) || (Verbose && !HoldErrs))
		nmessage(Arpa_Info, ">>> %s", buf);
	else if (CurEnv->e_xfp != NULL)
		fprintf(CurEnv->e_xfp, ">>> %s\n", buf);
	if (!SmtpClosing)
		fprintf(SmtpOut, "%s%s", buf, m->m_eol);
}

# endif SMTP
