/*	main.c	4.4	82/11/15	*/

/*
 * TFTP User Program -- Command Interface.
 */
#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <ctype.h>
#include <netdb.h>

struct	sockaddr_in sin;
int	f;
int	trace;
int	verbose;
int	connected;
char	mode[32];
char	line[200];
int	margc;
char	*margv[20];
char	*prompt = "tftp";
jmp_buf	toplevel;
int	intr();
struct	servent *sp;

int	quit(), help(), setverbose(), settrace(), status();
int	get(), put(), setpeer(), setmode();

#define HELPINDENT (sizeof("connect"))

struct cmd {
	char	*name;
	char	*help;
	int	(*handler)();
};

char	vhelp[] = "toggle verbose mode";
char	thelp[] = "toggle packet tracing";
char	chelp[] = "connect to remote tftp";
char	qhelp[] = "exit tftp";
char	hhelp[] = "print help information";
char	shelp[] = "send file";
char	rhelp[] = "receive file";
char	mhelp[] = "set file transfer mode";
char	sthelp[] = "show current status";

struct cmd cmdtab[] = {
	{ "connect",	chelp,		setpeer },
	{ "mode",	mhelp,		setmode },
	{ "put",	shelp,		put },
	{ "get",	rhelp,		get },
	{ "quit",	qhelp,		quit },
	{ "verbose",	vhelp,		setverbose },
	{ "trace",	thelp,		settrace },
	{ "status",	sthelp,		status },
	{ "?",		hhelp,		help },
	0
};

struct	cmd *getcmd();
char	*tail();
char	*index();
char	*rindex();

main(argc, argv)
	char *argv[];
{
	sp = getservbyname("tftp", "udp");
	if (sp == 0) {
		fprintf(stderr, "tftp: udp/tftp: unknown service\n");
		exit(1);
	}
	f = socket(AF_INET, SOCK_DGRAM, 0, 0);
	if (f < 0) {
		perror("socket");
		exit(3);
	}
	strcpy(mode, "netascii");
	if (argc > 1) {
		if (setjmp(toplevel) != 0)
			exit(0);
		setpeer(argc, argv);
	}
	setjmp(toplevel);
	for (;;)
		command(1);
}

char	*hostname;
char	hnamebuf[32];

setpeer(argc, argv)
	int argc;
	char *argv[];
{
	register int c;
	struct hostent *host;

	if (argc < 2) {
		strcpy(line, "Connect ");
		printf("(to) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc > 3) {
		printf("usage: %s host-name [port]\n", argv[0]);
		return;
	}
	host = gethostbyname(argv[1]);
	if (host) {
		sin.sin_family = host->h_addrtype;
		bcopy(host->h_addr, &sin.sin_addr, host->h_length);
		hostname = host->h_name;
	} else {
		sin.sin_family = AF_INET;
		sin.sin_addr.s_addr = inet_addr(argv[1]);
		if (sin.sin_addr.s_addr == -1) {
			connected = 0;
			printf("%s: unknown host\n", argv[1]);
			return;
		}
		strcpy(hnamebuf, argv[1]);
		hostname = hnamebuf;
	}
	sin.sin_port = sp->s_port;
	if (argc == 3) {
		sin.sin_port = atoi(argv[2]);
		if (sin.sin_port < 0) {
			printf("%s: bad port number\n", argv[2]);
			connected = 0;
			return;
		}
	}
	sin.sin_port = htons((u_short)sin.sin_port);
	connected = 1;
}

struct	modes {
	char *m_name;
	char *m_mode;
} modes[] = {
	{ "ascii",	"netascii" },
	{ "binary",	"octect" },
	{ "mail",	"mail" },
	{ 0,		0 }
};

setmode(argc, argv)
	char *argv[];
{
	register struct modes *p;

	if (argc > 2) {
		char *sep;

		printf("usage: %s [", argv[0]);
		sep = " ";
		for (p = modes; p->m_name; p++) {
			printf("%s%s", sep, p->m_name);
			if (*sep == ' ')
				sep = " | ";
		}
		printf(" ]\n");
		return;
	}
	if (argc < 2) {
		printf("Using %s mode to transfer files.\n", mode);
		return;
	}
	for (p = modes; p->m_name; p++)
		if (strcmp(argv[1], p->m_name) == 0)
			break;
	if (p->m_name)
		strcpy(mode, p->m_mode);
	else
		printf("%s: unknown mode\n", argv[1]);
}

/*
 * Send file(s).
 */
put(argc, argv)
	char *argv[];
{
	int fd;
	register int n, addr;
	register char *cp, *targ;

	if (argc < 2) {
		strcpy(line, "send ");
		printf("(file) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		putusage(argv[0]);
		return;
	}
	targ = argv[argc - 1];
	if (index(argv[argc - 1], ':')) {
		char *cp;
		struct hostent *hp;

		for (n = 1; n < argc - 1; n++)
			if (index(argv[n], ':')) {
				putusage(argv[0]);
				return;
			}
		cp = argv[argc - 1];
		targ = index(cp, ':');
		*targ++ = 0;
		hp = gethostbyname(cp);
		if (hp == 0) {
			printf("%s: Unknown host.\n", cp);
			return;
		}
		bcopy(hp->h_addr, (caddr_t)&sin.sin_addr, hp->h_length);
		sin.sin_family = hp->h_addrtype;
		connected = 1;
		hostname = hp->h_name;
	}
	if (!connected) {
		printf("No target machine specified.\n");
		return;
	}
	sigset(SIGINT, intr);
	if (argc < 4) {
		cp = argc == 2 ? tail(targ) : argv[1];
		fd = open(cp);
		if (fd < 0) {
			perror(cp);
			return;
		}
		sendfile(fd, targ);
		return;
	}
	cp = index(targ, '\0'); 
	*cp++ = '/';
	for (n = 1; n < argc - 1; n++) {
		strcpy(cp, tail(argv[n]));
		fd = open(argv[n], 0);
		if (fd < 0) {
			perror(argv[n]);
			continue;
		}
		sendfile(fd, targ);
	}
}

putusage(s)
	char *s;
{
	printf("usage: %s file ... host:target, or\n", s);
	printf("       %s file ... target (when already connected)\n", s);
}

/*
 * Receive file(s).
 */
get(argc, argv)
	char *argv[];
{
	int fd;
	register int n, addr;
	register char *cp;
	char *src;

	if (argc < 2) {
		strcpy(line, "get ");
		printf("(files) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		getusage(argv[0]);
		return;
	}
	if (!connected)
		for (n = 1; n < argc - 1; n++)
			if (index(argv[n], ':') == 0) {
				getusage(argv[0]);
				return;
			}
	sigset(SIGINT, intr);
	for (n = 1; argc == 2 || n < argc - 1; n++) {
		src = index(argv[n], ':');
		if (src == NULL)
			src = argv[n];
		else {
			struct hostent *hp;

			*src++ = 0;
			hp = gethostbyname(argv[n]);
			if (hp == 0) {
				printf("%s: Unknown host.\n", argv[n]);
				continue;
			}
			bcopy(hp->h_addr, (caddr_t)&sin.sin_addr, hp->h_length);
			sin.sin_family = hp->h_addrtype;
			connected = 1;
			hostname = hp->h_name;
		}
		if (argc < 4) {
			cp = argc == 3 ? argv[2] : tail(src);
			fd = creat(cp, 0644);
			if (fd < 0) {
				perror(cp);
				return;
			}
			recvfile(fd, src);
			break;
		}
		cp = index(argv[argc - 1], '\0');
		*cp++ = '/';
		strcpy(cp, tail(src));
		fd = creat(src, 0644);
		if (fd < 0) {
			perror(src);
			continue;
		}
		recvfile(fd, src);
	}
}

getusage(s)
{
	printf("usage: %s host:file host:file ... file, or\n", s);
	printf("       %s file file ... file if connected\n", s);
}

status(argc, argv)
	char *argv[];
{
	if (connected)
		printf("Connected to %s.\n", hostname);
	else
		printf("Not connected.\n");
	printf("Mode: %s Verbose: %s Tracing: %s\n", mode,
		verbose ? "on" : "off", trace ? "on" : "off");
}

intr()
{
	longjmp(toplevel, -1);
}

char *
tail(filename)
	char *filename;
{
	register char *s;
	
	while (*filename) {
		s = rindex(filename, '/');
		if (s == NULL)
			break;
		if (s[1])
			return (s + 1);
		*s = '\0';
	}
	return (filename);
}

/*
 * Command parser.
 */
command(top)
	int top;
{
	register struct cmd *c;

	if (!top)
		putchar('\n');
	else
		sigset(SIGINT, SIG_DFL);
	for (;;) {
		printf("%s> ", prompt);
		if (gets(line) == 0)
			break;
		if (line[0] == 0)
			break;
		makeargv();
		c = getcmd(margv[0]);
		if (c == (struct cmd *)-1) {
			printf("?Ambiguous command\n");
			continue;
		}
		if (c == 0) {
			printf("?Invalid command\n");
			continue;
		}
		(*c->handler)(margc, margv);
		if (c->handler != help)
			break;
	}
	longjmp(toplevel, 1);
}

struct cmd *
getcmd(name)
	register char *name;
{
	register char *p, *q;
	register struct cmd *c, *found;
	register int nmatches, longest;

	longest = 0;
	nmatches = 0;
	found = 0;
	for (c = cmdtab; p = c->name; c++) {
		for (q = name; *q == *p++; q++)
			if (*q == 0)		/* exact match? */
				return (c);
		if (!*q) {			/* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = c;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)
		return ((struct cmd *)-1);
	return (found);
}

/*
 * Slice a string up into argc/argv.
 */
makeargv()
{
	register char *cp;
	register char **argp = margv;

	margc = 0;
	for (cp = line; *cp;) {
		while (isspace(*cp))
			cp++;
		if (*cp == '\0')
			break;
		*argp++ = cp;
		margc += 1;
		while (*cp != '\0' && !isspace(*cp))
			cp++;
		if (*cp == '\0')
			break;
		*cp++ = '\0';
	}
	*argp++ = 0;
}

/*VARARGS*/
quit()
{
	exit(0);
}

/*
 * Help command.
 */
help(argc, argv)
	int argc;
	char *argv[];
{
	register struct cmd *c;

	if (argc == 1) {
		printf("Commands may be abbreviated.  Commands are:\n\n");
		for (c = cmdtab; c->name; c++)
			printf("%-*s\t%s\n", HELPINDENT, c->name, c->help);
		return;
	}
	while (--argc > 0) {
		register char *arg;
		arg = *++argv;
		c = getcmd(arg);
		if (c == (struct cmd *)-1)
			printf("?Ambiguous help command %s\n", arg);
		else if (c == (struct cmd *)0)
			printf("?Invalid help command %s\n", arg);
		else
			printf("%s\n", c->help);
	}
}

/*
 * Call routine with argc, argv set from args (terminated by 0).
 */
/* VARARGS2 */
call(routine, args)
	int (*routine)();
	int args;
{
	register int *argp;
	register int argc;

	for (argc = 0, argp = &args; *argp++ != 0; argc++)
		;
	(*routine)(argc, &args);
}

/*VARARGS*/
settrace()
{
	trace = !trace;
	printf("Packet tracing %s.\n", trace ? "on" : "off");
}

/*VARARGS*/
setverbose()
{
	verbose = !verbose;
	printf("Verbose mode %s.\n", verbose ? "on" : "off");
}
