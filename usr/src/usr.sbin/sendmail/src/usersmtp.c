# include <ctype.h>
# include <sysexits.h>
# include "sendmail.h"

# ifndef SMTP
SCCSID(@(#)usersmtp.c	3.22		%G%	(no SMTP));
# else SMTP

SCCSID(@(#)usersmtp.c	3.22		%G%);

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

# define REPLYTYPE(r)	((r) / 100)
# define REPLYCLASS(r)	(((r) / 10) % 10)

static FILE	*SmtpOut;	/* output file */
static FILE	*SmtpIn;	/* input file */
static int	SmtpPid;	/* pid of mailer */
static int	SmtpErrstat;	/* error status if open fails */

smtpinit(m, pvp, ctladdr)
	struct mailer *m;
	char **pvp;
	ADDRESS *ctladdr;
{
	register int r;
	char buf[MAXNAME];
	extern tick();
	extern char *canonname();

	/*
	**  Open the connection to the mailer.
	*/

	SmtpIn = SmtpOut = NULL;
	SmtpPid = openmailer(m, pvp, ctladdr, TRUE, &SmtpOut, &SmtpIn);
	if (SmtpPid < 0)
	{
		SmtpErrstat = ExitStat;
# ifdef DEBUG
		if (tTd(18, 1))
			printf("smtpinit: cannot open: Errstat %d errno %d\n",
			   SmtpErrstat, errno);
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
	smtpmessage("MAIL From:<%s>", canonname(buf));
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

	if (SmtpPid < 0)
		return (SmtpErrstat);

	smtpmessage("RCPT To:<%s>", canonname(to->q_user));

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

	if (SmtpPid < 0)
		return (SmtpErrstat);

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
	(*e->e_puthdr)(SmtpOut, m, CurEnv);
	fprintf(SmtpOut, "\n");
	(*e->e_putbody)(SmtpOut, m, TRUE);
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
**		showresp -- if set, give a response message.
**
**	Returns:
**		none.
**
**	Side Effects:
**		sends the final protocol and closes the connection.
*/

smtpquit(name, showresp)
	char *name;
	bool showresp;
{
	register int i;

	if (SmtpPid < 0)
		return;
	smtpmessage("QUIT");
	(void) reply();
	(void) fclose(SmtpIn);
	(void) fclose(SmtpOut);
	i = endmailer(SmtpPid, name);
	if (showresp)
		giveresponse(i, TRUE, LocalMailer);
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
		char buf[MAXLINE];
		register int r;
		register char *p;

		/* actually do the read */
		(void) fflush(Xscript);			/* for debugging */
		p = sfgets(buf, sizeof buf, SmtpIn);

		if (p == NULL)
			return (-1);

		/* log the input in the transcript for future error returns */
		if (Verbose && !HoldErrs)
			fputs(buf, stdout);
		fputs(buf, Xscript);

		/* if continuation is required, we can go on */
		if (buf[3] == '-' || !isdigit(buf[0]))
			continue;

		/* decode the reply code */
		r = atoi(buf);

		/* extra semantics: 0xx codes are "informational" */
		if (r < 100)
			continue;

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
		printf(">>> %s\n", buf);
	fprintf(Xscript, ">>> %s\n", buf);
	fprintf(SmtpOut, "%s\r\n", buf);
}

# endif SMTP
