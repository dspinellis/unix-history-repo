# include <stdio.h>
# include <signal.h>
# include <ctype.h>
# include <sgtty.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <netinet/in.h>
# include <netdb.h>

static char	SccsId[] =	"@(#)mconnect.c	4.1		7/25/83";

struct sockaddr_in	SendmailAddress;
struct sgttyb		TtyBuf;

main(argc, argv)
	int argc;
	char **argv;
{
	register int s;
	char *host;
	register int i;
	int pid;
	struct servent *sp;
	int raw = 0;
	char buf[1000];
	register char *p;
	extern char *index();
	register FILE *f;
	extern u_long inet_addr();
	extern finis();

	(void) gtty(0, &TtyBuf);
	(void) signal(SIGINT, finis);
	s = socket(AF_INET, SOCK_STREAM, 0, 0);
	if (s < 0)
	{
		perror("socket");
		exit(-1);
	}

	sp = getservbyname("smtp", "tcp");
	if (sp != NULL)
		SendmailAddress.sin_port = sp->s_port;

	while (--argc > 0)
	{
		register char *p = *++argv;

		if (*p == '-')
		{
			switch (*++p)
			{
			  case 'h':		/* host */
				break;

			  case 'p':		/* port */
				SendmailAddress.sin_port = htons(atoi(*++argv));
				argc--;
				break;

			  case 'r':		/* raw connection */
				raw = 1;
				TtyBuf.sg_flags &= ~CRMOD;
				stty(0, &TtyBuf);
				TtyBuf.sg_flags |= CRMOD;
				break;
			}
		}
		else if (host == NULL)
			host = p;
	}
	if (host == NULL)
		host = "localhost";

	if (isdigit(*host))
		SendmailAddress.sin_addr.s_addr = inet_addr(host);
	else
	{
		register struct hostent *hp = gethostbyname(host);

		if (hp == NULL)
		{
			fprintf(stderr, "mconnect: unknown host %s\r\n", host);
			finis();
		}
		bcopy(hp->h_addr, &SendmailAddress.sin_addr, hp->h_length);
	}
	SendmailAddress.sin_family = AF_INET;
	printf("connecting to host %s (0x%x), port 0x%x\r\n", host,
	       SendmailAddress.sin_addr.s_addr, SendmailAddress.sin_port);
	if (connect(s, &SendmailAddress, sizeof SendmailAddress, 0) < 0)
	{
		perror("connect");
		exit(-1);
	}

	/* good connection, fork both sides */
	printf("connection open\n");
	pid = fork();
	if (pid < 0)
	{
		perror("fork");
		exit(-1);
	}
	if (pid == 0)
	{
		/* child -- standard input to sendmail */
		int c;

		f = fdopen(s, "w");
		while ((c = fgetc(stdin)) >= 0)
		{
			if (!raw && c == '\n')
				fputc('\r', f);
			fputc(c, f);
			if (c == '\n')
				fflush(f);
		}
	}
	else
	{
		/* parent -- sendmail to standard output */
		f = fdopen(s, "r");
		while (fgets(buf, sizeof buf, f) != NULL)
		{
			fputs(buf, stdout);
			fflush(stdout);
		}
	}
	finis();
}

finis()
{
	stty(0, &TtyBuf);
	exit(0);
}
