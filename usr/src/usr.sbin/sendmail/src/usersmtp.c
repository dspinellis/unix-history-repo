# include <ctype.h>
# include <sysexits.h>
# include "sendmail.h"

# ifndef SMTP
SCCSID(@(#)usersmtp.c	3.32		%G%	(no SMTP));
# else SMTP

SCCSID(@(#)usersmtp.c	3.32		%G%);



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
**		ctladdr -- controlling address for this mailer.
**
**	Returns:
**		appropriate exit status -- EX_OK on success.
**
**	Side Effects:
**		creates connection and sends initial protocol.
*/

smtpinit(m, pvp, ctladdr)
	struct mailer *m;
	char **pvp;
	ADDRESS *ctladdr;
{
	register int r;
	char buf[MAXNAME];
	extern char *canonname();

	/*
	**  Open the connection to the mailer.
	*/

	SmtpIn = SmtpOut = NULL;
	SmtpPid = openmailer(m, pvp, ctladdr, TRUE, &SmtpOut, &SmtpIn);
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

	r = reply();
	if (r < 0 || REPLYTYPE(r) != 2)
		return (EX_TEMPFAIL);

	/*
	**  Send the HELO command.
	**	My mother taught me to always introduce myself.
	*/

	smtpmessage("HELO %s", HostName);
	r = reply();
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
		smtpmessage("VERB");
		r = reply();
		if (r < 0)
			return (EX_TEMPFAIL);

		/* tell it we will be sending one transaction only */
		smtpmessage("ONEX");
		r = reply();
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
	    !bitset(M_FULLSMTP, m->m_flags))
	{
		smtpmessage("MAIL From:<%s>", canonname(buf, 1));
	}
	else
	{
		smtpmessage("MAIL From:<@%s%c%s>", HostName,
			    buf[0] == '@' ? ',' : ':', canonname(buf, 1));
	}
	r = reply();
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
**
**	Returns:
**		exit status corresponding to recipient status.
**
**	Side Effects:
**		Sends the mail via SMTP.
*/

smtprcpt(to)
	ADDRESS *to;
{
	register int r;
	extern char *canonname();

	smtpmessage("RCPT To:<%s>", canonname(to->q_user, 2));

	r = reply();
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
**  SMTPFINISH -- finish up sending all the SMTP protocol.
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

smtpfinish(m, e)
	struct mailer *m;
	register ENVELOPE *e;
{
	register int r;

	/*
	**  Send the data.
	**	Dot hiding is done here.
	*/

	smtpmessage("DATA");
	r = reply();
	if (r < 0 || REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	else if (r == 554)
		return (EX_UNAVAILABLE);
	else if (r != 354)
		return (EX_PROTOCOL);
	(*e->e_puthdr)(SmtpOut, m, CurEnv, TRUE);
	fprintf(SmtpOut, "\r\n");
	(*e->e_putbody)(SmtpOut, m, TRUE, CurEnv, TRUE);
	smtpmessage(".");
	r = reply();
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

smtpquit(name)
	char *name;
{
	int i;

	if (SmtpClosing)
	{
		SmtpClosing = FALSE;
		return;
	}

	smtpmessage("QUIT");
	i = reply();
	if (i != 221)
		syserr("smtpquit %s: reply %d", name, i);
	(void) fclose(SmtpIn);
	(void) fclose(SmtpOut);
	i = endmailer(SmtpPid, name);
	if (i != EX_OK)
		syserr("smtpquit %s: stat %d", name, i);
}
/*
**  REPLY -- read arpanet reply
**
**	Parameters:
**		none.
**
**	Returns:
**		reply code it reads.
**
**	Side Effects:
**		flushes the mail file.
*/

reply()
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
			return (-1);
		fixcrlf(SmtpReplyBuffer, TRUE);

		/* log the input in the transcript for future error returns */
		if (Verbose && !HoldErrs)
			nmessage(Arpa_Info, "%s", SmtpReplyBuffer);
		if (CurEnv->e_xfp != NULL)
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
			smtpquit("SMTP Shutdown");
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
**		a, b, c -- parameters
**
**	Returns:
**		none.
**
**	Side Effects:
**		writes message to SmtpOut.
*/

/*VARARGS1*/
smtpmessage(f, a, b, c)
	char *f;
{
	char buf[100];

	(void) sprintf(buf, f, a, b, c);
	if (tTd(18, 1) || (Verbose && !HoldErrs))
		nmessage(Arpa_Info, ">>> %s", buf);
	if (CurEnv->e_xfp != NULL)
		fprintf(CurEnv->e_xfp, ">>> %s\n", buf);
	if (!SmtpClosing)
		fprintf(SmtpOut, "%s\r\n", buf);
}

# endif SMTP
