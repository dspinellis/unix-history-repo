# include <ctype.h>
# include <wellknown.h>
# include <sysexits.h>
# include <stdio.h>
# include <useful.h>

static char	SccsId[] =	"@(#)usersmtp.c	3.2	%G%";

/*
**  TCP -- TCP/Ethernet/ARPAnet mailer
**
**	This arranges to send a message over the TCP connection.
*/

# define MAXLINE	200

char	*MailCommand =	"/usr/lib/sendmail";
char	*MailUser =	"network";
char	*MailPassword =	"mailhack";
FILE	*InConnection;
FILE	*OutConnection;
bool	Verbose;
bool	Debug;

main(argc, argv)
	int argc;
	char **argv;
{
	register int stat;

	while (argc > 1 && argv[1][0] == '-')
	{
		register char *p = *++argv;

		argc--;
		switch (p[1])
		{
		  case 'v':
			Verbose = TRUE;
			break;

		  case 'd':
			Debug = TRUE;
			break;
		}
	}

	if (argc < 4)
	{
		if (Debug)
			printf("Usage\n");
		exit(EX_USAGE);
	}

	if (openconnection(argv[2]) < 0)
		exit(EX_TEMPFAIL);

	stat = runsmtp(argv[1], &argv[3]);

	if (Debug)
		printf("Finishing with stat %d\n", stat);

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

openconnection(host)
	char *host;
{
	char cmdbuf[100];
	register int fd;

	/* create the command name */
	sprintf(cmdbuf, "%s -as%s%s", MailCommand,
					Verbose ? " -v" : "",
					Debug ? " -d" : "");

	if (Debug)
		printf("Creating connection to \"%s\" on %s\n", cmdbuf, host);

	/* create connection (we hope) */
	fd = rexec(&host, SHELLSERVER, cmdbuf, MailUser, MailPassword);
	if (fd < 0)
		return (-1);
	InConnection = fdopen(fd, "r");
	OutConnection = fdopen(fd, "w");
	if (InConnection == NULL || OutConnection == NULL)
		return (-1);

	if (Debug)
		printf("Connection open to %s\n", host);

	return (0);
}
/*
**  RUNSMTP -- run the SMTP protocol over connection.
**
**	Parameters:
**		fr -- from person.
**		tolist -- list of recipients.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sends the mail via SMTP.
*/

runsmtp(fr, tolist)
	char *fr;
	char **tolist;
{
	register int r;
	register char **t;
	char buf[MAXLINE];

	/* get greeting message */
	r = reply();
	if (r / 100 != 2)
		return (EX_TEMPFAIL);

	/* send the mail command */
	message("MAIL From:<%s>\r\n", fr);
	r = reply();
	if (r != 250)
		return (EX_SOFTWARE);

	/* send the recipients */
	for (t = tolist; *t != NULL; t++)
	{
		message("MRCP To:<%s>\r\n", *t);
		r = reply();
		if (r != 250)
			return (EX_NOUSER);
	}

	/* send the data */
	message("DATA\r\n");
	r = reply();
	if (r != 354)
		return (EX_SOFTWARE);
	while (fgets(buf, sizeof buf, stdin) != NULL)
	{
		/* change trailing newline to crlf */
		register char *p = index(buf, '\n');

		if (p != NULL)
			*p = '\0';
		if (buf[0] == '.')
			message(".");
		message("%s\r\n", buf);
	}
	message(".\r\n");
	r = reply();
	if (r != 250)
		return (EX_SOFTWARE);

	/* force delivery */
	message("DOIT\r\n");
	r = reply();
	if (r != 250)
		return (EX_TEMPFAIL);

	message("QUIT\r\n");
	r = reply();
	if (r != 221)
		return (EX_SOFTWARE);

	return (EX_OK);
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
	fflush(OutConnection);

	if (Debug)
		printf("reply\n");

	/* read the input line */
	for (;;)
	{
		char buf[MAXLINE];
		register int r;

		if (fgets(buf, sizeof buf, InConnection) == NULL)
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
**  MESSAGE -- send message to server
**
**	Parameters:
**		f -- format
**		a, b, c -- parameters
**
**	Returns:
**		none.
**
**	Side Effects:
**		writes message to OutChannel.
*/

message(f, a, b, c)
	char *f;
{
	char buf[100];

	sprintf(buf, f, a, b, c);
	if (Debug)
		fputs(buf, stdout);
	fputs(buf, OutConnection);
}
