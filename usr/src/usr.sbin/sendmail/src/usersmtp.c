# include <wellknown.h>
# include <sysexits.h>
# include <stdio.h>
# include <useful.h>

static char	SccsId[] =	"@(#)usersmtp.c	3.1	%G%";

/*
**  TCP -- TCP/Ethernet/ARPAnet mailer
**
**	This arranges to send a message over the TCP connection.
*/

# define MAXLINE	200

char	*MailCommand =	"/usr/lib/sendmail";
char	*MailUser =	"network";
char	*MailPassword =	NULL;
FILE	*MailFile;
bool	Verbose;

main(argc, argv)
	int argc;
	char **argv;
{
	extern FILE *openconnection();
	register int stat;

	if (argc < 4)
		exit(EX_USAGE);

	MailFile = openconnection(argv[2]);
	if (MailFile == NULL)
		exit(EX_TEMPFAIL);

	stat = runsmtp(argv[1], &argv[3]);

	exit(stat);
}
/*
**  OPENCONNECTION -- open connection to SMTP socket
**
**	Parameters:
**		none.
**
**	Returns:
**		file pointer of connection.
**		NULL on error.
**
**	Side Effects:
**		none.
*/

FILE *
openconnection(host)
	char *host;
{
	char cmdbuf[100];
	extern FILE *rexec();
	register FILE *f;

	/* create the command name */
	sprintf(cmdbuf, "%s -p", MailCommand);

	/* create connection (we hope) */
	f = rexec(&host, SHELLSERVER, cmdbuf, &MailUser, NULL);

	return (f);
}
/*
**  RUNSMTP -- run the SMTP protocol over connection.
**
**	Parameters:
**		fr -- from person.
**		tolist -- list of recipients.
**		mf -- mail connection file.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sends the mail via SMTP.
*/

runsmtp(fr, tolist, mf)
	char *fr;
	char **tolist;
	FILE *mf;
{
	register int r;
	register char **t;
	char buf[MAXLINE];

	/* get greeting message */
	r = reply(mf);
	if (r / 100 != 2)
		return (EX_TEMPFAIL);

	/* send the mail command */
	fprintf(mf, "MAIL From:<%s>\r\n", fr);
	r = reply(mf);
	if (r != 250)
		return (EX_SOFTWARE);

	/* send the recipients */
	for (t = tolist; *t != NULL; t++)
	{
		fprintf(mf, "MRCP To:<%s>\r\n", *t);
		r = reply(mf);
		if (r != 250)
			return (EX_NOUSER);
	}

	/* send the data */
	fprintf(mf, "DATA\r\n");
	r = reply(mf);
	if (r != 354)
		return (EX_SOFTWARE);
	while (fgets(buf, sizeof buf, stdin) != NULL)
	{
		/* change trailing newline to crlf */
		register char *p = index(buf, '\n');

		if (p != NULL)
			*p = '\0';
		if (buf[0] == '.')
			fprintf(mf, ".");
		fprintf(mf, "%s\r\n", buf);
	}
	fprintf(mf, ".\r\n");
	r = reply(mf);
	if (r != 250)
		return (EX_SOFTWARE);

	/* force delivery */
	fprintf(mf, "DOIT\r\n");
	r = reply(mf);
	if (r != 250)
		return (EX_TEMPFAIL);

	fprintf(mf, "QUIT\r\n");
	r = reply(mf);
	if (r != 221)
		return (EX_SOFTWARE);

	return (EX_OK);
}
/*
**  REPLY -- read arpanet reply
**
**	Parameters:
**		mf -- mail file.
**
**	Returns:
**		reply code it reads.
**
**	Side Effects:
**		flushes the mail file.
*/

reply(mf)
	FILE *mf;
{
	fflush(mf);

	/* read the input line */
	for (;;)
	{
		char buf[MAXLINE];
		register int r;

		if (fgets(buf, sizeof buf, mf) == NULL)
			return (-1);
		if (Verbose)
			fputs(buf, stdout);
		if (buf[3] == '-')
			continue;
		r = atoi(buf);
		if (r < 100)
			continue;
		return (r);
	}
}
