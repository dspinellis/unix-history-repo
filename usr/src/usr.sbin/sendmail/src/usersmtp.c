# include <ctype.h>
# include <wellknown.h>
# include <sysexits.h>
# include "sendmail.h"

static char	SccsId[] =	"@(#)usersmtp.c	3.4	%G%";

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

static FILE	*SmtpOut;	/* output file */
static FILE	*SmtpIn;	/* input file */
static int	SmtpPid;	/* pid of mailer */

smtpinit(m, pvp, ctladdr)
	struct mailer *m;
	char **pvp;
	ADDRESS *ctladdr;
{
	register int r;
	char buf[MAXNAME];

	/*
	**  Open the connection to the mailer.
	*/

	SmtpPid = openmailer(m, pvp, ctladdr, TRUE, &SmtpOut, &SmtpIn);

	/*
	**  Get the greeting message.
	**	This should appear spontaneously.
	*/

	r = reply();
	if (REPLYTYPE(r) != 2)
		return (EX_TEMPFAIL);

	/*
	**  Send the MAIL command.
	**	Designates the sender.
	*/

	(void) expand("$g", buf, &buf[sizeof buf - 1]);
	smtpmessage("MAIL From:<%s>", buf);
	r = reply();
	if (REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	if (r != 250)
		return (EX_SOFTWARE);
	return (EX_OK);
}
/*
**  SMTPMRCP -- designate recipient.
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

smtpmrcp(to)
	ADDRESS *to;
{
	register int r;

	smtpmessage("MRCP To:<%s>", to->q_user);

	r = reply();
	if (REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	if (r != 250)
		return (EX_NOUSER);

	return (EX_OK);
}
/*
**  SMTPFINISH -- finish up sending all the SMTP protocol.
**
**	Parameters:
**		m -- mailer being sent to.
**		editfcn -- a function to call to output the
**			text of the message with.
**
**	Returns:
**		exit status corresponding to DOIT command.
**
**	Side Effects:
**		none.
*/

smtpfinish(m, editfcn)
	struct mailer *m;
	int (*editfcn)();
{
	register int r;

	/*
	**  Send the data.
	**	Dot hiding is done here.
	*/

	smtpmessage("DATA");
	r = reply();
	if (REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	if (r != 354)
		return (EX_SOFTWARE);
	(*editfcn)(SmtpOut, m, TRUE);
	smtpmessage(".");
	r = reply();
	if (REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	if (r != 250)
		return (EX_SOFTWARE);

	/*
	**  Make the actual delivery happen.
	*/

	smtpmessage("DOIT");
	r = reply();
	if (r != 250)
		return (EX_TEMPFAIL);

	return (EX_OK);
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
	register int i;

	smtpmessage("QUIT");
	(void) reply();
	(void) fclose(SmtpIn);
	(void) fclose(SmtpOut);
	i = endmailer(SmtpPid, name);
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

	if (Debug)
		printf("reply\n");

	/* read the input line */
	for (;;)
	{
		char buf[MAXLINE];
		register int r;

		if (fgets(buf, sizeof buf, SmtpIn) == NULL)
			return (-1);
		if (Verbose)
			fputs(buf, stdout);
		if (buf[3] == '-' || !isdigit(buf[0]))
			continue;
		r = atoi(buf);
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
	strcat(buf, "\r\n");
	if (Debug)
		fputs(buf, stdout);
	fputs(buf, SmtpOut);
}
