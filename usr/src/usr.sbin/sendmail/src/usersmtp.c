# include <ctype.h>
# include <wellknown.h>
# include <sysexits.h>
# include <stdio.h>
# include <useful.h>

static char	SccsId[] =	"@(#)usersmtp.c	3.3	%G%";

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
int	Status;			/* exit status */

main(argc, argv)
	int argc;
	char **argv;
{
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
		exit(Status);

	Status = runsmtp(argv[1], &argv[3]);

	closeconnection();

	if (Debug)
		printf("Finishing with stat %d\n", Status);

	exit(Status);
}
/*
**  OPENCONNECTION -- open connection to SMTP socket
**
**	Parameters:
**		host -- the name of the host to connect to.  This
**			will be replaced by the canonical name of
**			the host.
**
**	Returns:
**		File descriptor of connection.
**		-1 on error.
**
**	Side Effects:
**		sets 'Status' to represent the problem on error.
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

	/* verify host name */
	if (rhost(&host) < 0)
	{
		if (Debug)
			printf("Unknown host %s\n", host);
		Status = EX_NOHOST;
		return (-1);
	}

	/* create connection (we hope) */
	fd = rexec(&host, SHELLSERVER, cmdbuf, MailUser, MailPassword);
	if (fd < 0)
	{
		Status = EX_TEMPFAIL;
		return (-1);
	}
	InConnection = fdopen(fd, "r");
	OutConnection = fdopen(fd, "w");
	if (InConnection == NULL || OutConnection == NULL)
	{
		Status = EX_SOFTWARE;
		return (-1);
	}

	if (Debug)
		printf("Connection open to %s\n", host);

	return (0);
}
/*
**	CLOSECONNECTION -- close the connection to the SMTP server.
**
**	This routine also sends a handshake.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Closes the connection.
*/

closeconnection()
{
	register int r;

	message("QUIT");
	r = reply();

	if (Debug)
		printf("Closing connection, reply = %d\n", r);
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

# define REPLYTYPE(r)	((r) / 100)

runsmtp(fr, tolist)
	char *fr;
	char **tolist;
{
	register int r;
	register char **t;
	char buf[MAXLINE];

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

	message("MAIL From:<%s>", fr);
	r = reply();
	if (REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	if (r != 250)
		return (EX_SOFTWARE);

	/*
	**  Send the recipients.
	*/

	for (t = tolist; *t != NULL; t++)
	{
		message("MRCP To:<%s>", *t);
		r = reply();
		if (REPLYTYPE(r) == 4)
			return (EX_TEMPFAIL);
		if (r != 250)
			return (EX_NOUSER);
	}

	/*
	**  Send the data.
	**	Dot hiding is done here.
	*/

	message("DATA");
	r = reply();
	if (REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	if (r != 354)
		return (EX_SOFTWARE);
	while (fgets(buf, sizeof buf, stdin) != NULL)
	{
		/* change trailing newline to crlf */
		register char *p = index(buf, '\n');

		if (p != NULL)
			*p = '\0';
		message("%s%s", buf[0] == '.' ? "." : "", buf);
	}
	message(".");
	r = reply();
	if (REPLYTYPE(r) == 4)
		return (EX_TEMPFAIL);
	if (r != 250)
		return (EX_SOFTWARE);

	/*
	**  Make the actual delivery happen.
	*/

	message("DOIT");
	r = reply();
	if (r != 250)
		return (EX_TEMPFAIL);

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
	strcat(buf, "\r\n");
	if (Debug)
		fputs(buf, stdout);
	fputs(buf, OutConnection);
}
