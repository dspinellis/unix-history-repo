static char sccsid[] = "@(#)telnet.c	4.12 (Berkeley) %G%";
/*
 * User telnet program.
 */
#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <sgtty.h>
#include <setjmp.h>
#include <netdb.h>

#define	TELOPTS
#include "telnet.h"

#define	ctrl(x)		((x) & 037)
#define	strip(x)	((x)&0177)

char	ttyobuf[BUFSIZ], *tfrontp = ttyobuf, *tbackp = ttyobuf;
char	netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;

char	hisopts[256];
char	myopts[256];

char	doopt[] = { IAC, DO, '%', 'c', 0 };
char	dont[] = { IAC, DONT, '%', 'c', 0 };
char	will[] = { IAC, WILL, '%', 'c', 0 };
char	wont[] = { IAC, WONT, '%', 'c', 0 };

int	connected;
int	net;
int	showoptions;
int	options;
char	*prompt;
char	escape = ctrl(']');

char	line[200];
int	margc;
char	*margv[20];

jmp_buf	toplevel;
jmp_buf	peerdied;

extern	int errno;

int	tn(), quit(), suspend(), bye(), help();
int	setescape(), status(), toggle(), setoptions();

#define HELPINDENT (sizeof ("connect"))

struct cmd {
	char	*name;
	char	*help;
	int	(*handler)();
};

char	ohelp[] = "connect to a site";
char	chelp[] = "close current connection";
char	qhelp[] = "exit telnet";
char	zhelp[] = "suspend telnet";
char	ehelp[] = "set escape character";
char	shelp[] = "print status information";
char	hhelp[] = "print help information";
char	phelp[] = "toggle viewing of options processing";

struct cmd cmdtab[] = {
	{ "open",	ohelp,		tn },
	{ "close",	chelp,		bye },
	{ "quit",	qhelp,		quit },
	{ "z",		zhelp,		suspend },
	{ "escape",	ehelp,		setescape },
	{ "status",	shelp,		status },
	{ "options",	phelp,		setoptions },
	{ "?",		hhelp,		help },
	0
};

struct sockaddr_in sin = { AF_INET };

int	intr(), deadpeer();
char	*control();
struct	cmd *getcmd();
struct	servent *sp;

struct	sgttyb ostbuf;
struct	tchars otchars;
int	odisc;

main(argc, argv)
	int argc;
	char *argv[];
{
	sp = getservbyname("telnet", "tcp");
	if (sp == 0) {
		fprintf(stderr, "telnet: tcp/telnet: unknown service\n");
		exit(1);
	}
	ioctl(0, TIOCGETP, (char *)&ostbuf);
	ioctl(0, TIOCGETC, (char *)&otchars);
	ioctl(0, TIOCGETD, (char *)&odisc);
	setbuf(stdin, 0);
	setbuf(stdout, 0);
	prompt = argv[0];
	if (argc > 1 && !strcmp(argv[1], "-d"))
		options = SO_DEBUG, argv++, argc--;
	if (argc != 1) {
		if (setjmp(toplevel) != 0)
			exit(0);
		tn(argc, argv);
	}
	setjmp(toplevel);
	for (;;)
		command(1);
}

char	*hostname;
char	hnamebuf[32];

tn(argc, argv)
	int argc;
	char *argv[];
{
	register int c;
	register struct hostent *host;

	if (connected) {
		printf("?Already connected to %s\n", hostname);
		return;
	}
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
		bcopy(host->h_addr, (caddr_t)&sin.sin_addr, host->h_length);
		hostname = host->h_name;
	} else {
		sin.sin_family = AF_INET;
		sin.sin_addr.s_addr = inet_addr(argv[1]);
		if (sin.sin_addr.s_addr == -1) {
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
			return;
		}
	}
	sin.sin_port = htons(sin.sin_port);
	net = socket(0, SOCK_STREAM, 0, 0);
	if (net < 0) {
		perror("telnet: socket");
		return;
	}
	sigset(SIGINT, intr);
	sigset(SIGPIPE, deadpeer);
	printf("Trying...\n");
	if (connect(net, (caddr_t)&sin, sizeof (sin), 0) < 0) {
		perror("telnet: connect");
		sigset(SIGINT, SIG_DFL);
		return;
	}
	connected++;
	call(status, "status", 0);
	if (setjmp(peerdied) == 0)
		telnet(net);
	fprintf(stderr, "Connection closed by foreign host.\n");
	exit(1);
}

/*
 * Print status about the connection.
 */
/*VARARGS*/
status()
{
	if (connected)
		printf("Connected to %s.\n", hostname);
	else
		printf("No connection.\n");
	printf("Escape character is '%s'.\n", control(escape));
}

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
suspend()
{
	register int save;

	save = mode(0);
	kill(0, SIGTSTP);
	/* reget parameters in case they were changed */
	ioctl(0, TIOCGETP, (char *)&ostbuf);
	ioctl(0, TIOCGETC, (char *)&otchars);
	ioctl(0, TIOCGETD, (char *)&odisc);
	(void) mode(save);
}

/*VARARGS*/
bye()
{
	int how = 2;
	register char *op;

	(void) mode(0);
	if (connected) {
#ifndef notdef
		ioctl(net, SIOCDONE, &how);
#endif
		printf("Connection closed.\n");
		close(net);
		connected = 0;
		/* reset his options */
		for (op = hisopts; op < &hisopts[256]; op++)
			*op = 0;
	}
}

/*VARARGS*/
quit()
{
	call(bye, "bye", 0);
	exit(0);
}

/*
 * Help command.
 * Call each command handler with argc == 0 and argv[0] == name.
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
 * VARARGS2
 */
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

mode(f)
	register int f;
{
	struct sgttyb stbuf;
	static int prevmode = 0;
	struct tchars tchars;
	int onoff, disc, old;

	if (prevmode == f)
		return (f);
	old = prevmode;
	prevmode = f;
	stbuf = ostbuf;
	tchars = otchars;
	switch (f) {

	case 0:
		disc = odisc;
		onoff = 0;
		break;

	case 1:
	case 2:
		stbuf.sg_flags |= CBREAK;
		if (f == 1)
			stbuf.sg_flags &= ~ECHO;
		else
			stbuf.sg_flags |= ECHO;
		tchars.t_intrc = tchars.t_quitc = -1;
		tchars.t_stopc = tchars.t_startc = -1;
		disc = OTTYDISC;
		onoff = 1;
	}
	ioctl(fileno(stdin), TIOCSETD, &disc);
	ioctl(fileno(stdin), TIOCSETC, &tchars);
	ioctl(fileno(stdin), TIOCSETN, &stbuf);
	ioctl(fileno(stdin), FIONBIO, &onoff);
	ioctl(fileno(stdout), FIONBIO, &onoff);
	return (old);
}

char	sibuf[BUFSIZ], *sbp;
char	tibuf[BUFSIZ], *tbp;
int	scc, tcc;

/*
 * Select from tty and network...
 */
telnet(s)
	int s;
{
	register int c;
	int tin = fileno(stdin), tout = fileno(stdout);
	int on = 1;

	(void) mode(2);
	ioctl(s, FIONBIO, &on);
	for (;;) {
		int ibits = 0, obits = 0;

		if (nfrontp - nbackp)
			obits |= (1 << s);
		else
			ibits |= (1 << tin);
		if (tfrontp - tbackp)
			obits |= (1 << tout);
		else
			ibits |= (1 << s);
		if (scc < 0 && tcc < 0)
			break;
		select(16, &ibits, &obits, 0, 0);
		if (ibits == 0 && obits == 0) {
			sleep(5);
			continue;
		}

		/*
		 * Something to read from the network...
		 */
		if (ibits & (1 << s)) {
			scc = read(s, sibuf, sizeof (sibuf));
			if (scc < 0 && errno == EWOULDBLOCK)
				scc = 0;
			else {
				if (scc <= 0)
					break;
				sbp = sibuf;
			}
		}

		/*
		 * Something to read from the tty...
		 */
		if (ibits & (1 << tin)) {
			tcc = read(tin, tibuf, sizeof (tibuf));
			if (tcc < 0 && errno == EWOULDBLOCK)
				tcc = 0;
			else {
				if (tcc <= 0)
					break;
				tbp = tibuf;
			}
		}

		while (tcc > 0) {
			register int c;

			if ((&netobuf[BUFSIZ] - nfrontp) < 2)
				break;
			c = *tbp++ & 0377, tcc--;
			if (strip(c) == escape) {
				command(0);
				tcc = 0;
				break;
			}
			*nfrontp++ = c;
		}
		if ((obits & (1 << s)) && (nfrontp - nbackp) > 0)
			netflush(s);
		if (scc > 0)
			telrcv();
		if ((obits & (1 << tout)) && (tfrontp - tbackp) > 0)
			ttyflush(tout);
	}
	(void) mode(0);
}

command(top)
	int top;
{
	register struct cmd *c;
	int oldmode, wasopen;

	oldmode = mode(0);
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
	if (!top) {
		if (!connected)
			longjmp(toplevel, 1);
		(void) mode(oldmode);
	}
}

/*
 * Telnet receiver states for fsm
 */
#define	TS_DATA		0
#define	TS_IAC		1
#define	TS_WILL		2
#define	TS_WONT		3
#define	TS_DO		4
#define	TS_DONT		5

telrcv()
{
	register int c;
	static int state = TS_DATA;

	while (scc > 0) {
		c = *sbp++ & 0377, scc--;
		switch (state) {

		case TS_DATA:
			if (c == IAC)
				state = TS_IAC;
			else
				*tfrontp++ = c;
			continue;

		case TS_IAC:
			switch (c) {
			
			case WILL:
				state = TS_WILL;
				continue;

			case WONT:
				state = TS_WONT;
				continue;

			case DO:
				state = TS_DO;
				continue;

			case DONT:
				state = TS_DONT;
				continue;

			case DM:
				ioctl(fileno(stdout), TIOCFLUSH, 0);
				break;

			case NOP:
			case GA:
				break;

			default:
				break;
			}
			state = TS_DATA;
			continue;

		case TS_WILL:
			printoption("RCVD", will, c, !hisopts[c]);
			if (!hisopts[c])
				willoption(c);
			state = TS_DATA;
			continue;

		case TS_WONT:
			printoption("RCVD", wont, c, hisopts[c]);
			if (hisopts[c])
				wontoption(c);
			state = TS_DATA;
			continue;

		case TS_DO:
			printoption("RCVD", doopt, c, !myopts[c]);
			if (!myopts[c])
				dooption(c);
			state = TS_DATA;
			continue;

		case TS_DONT:
			printoption("RCVD", dont, c, myopts[c]);
			if (myopts[c]) {
				myopts[c] = 0;
				sprintf(nfrontp, wont, c);
				nfrontp += sizeof (wont) - 2;
				printoption("SENT", wont, c);
			}
			state = TS_DATA;
			continue;
		}
	}
}

willoption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_ECHO:
		(void) mode(1);

	case TELOPT_SGA:
		hisopts[option] = 1;
		fmt = doopt;
		break;

	case TELOPT_TM:
		fmt = dont;
		break;

	default:
		fmt = dont;
		break;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (dont) - 2;
	printoption("SENT", fmt, option);
}

wontoption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_ECHO:
		(void) mode(2);

	case TELOPT_SGA:
		hisopts[option] = 0;
		fmt = dont;
		break;

	default:
		fmt = dont;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (doopt) - 2;
	printoption("SENT", fmt, option);
}

dooption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_TM:
		fmt = wont;
		break;

	case TELOPT_SGA:
		fmt = will;
		break;

	default:
		fmt = wont;
		break;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (doopt) - 2;
	printoption("SENT", fmt, option);
}

/*
 * Set the escape character.
 */
setescape(argc, argv)
	int argc;
	char *argv[];
{
	register char *arg;
	char buf[50];

	if (argc > 2)
		arg = argv[1];
	else {
		printf("new escape character: ");
		gets(buf);
		arg = buf;
	}
	if (arg[0] != '\0')
		escape = arg[0];
	printf("Escape character is '%s'.\n", control(escape));
}

/*VARARGS*/
setoptions()
{
	showoptions = !showoptions;
	printf("%s show option processing.\n", showoptions ? "Will" : "Wont");
}

/*
 * Construct a control character sequence
 * for a special character.
 */
char *
control(c)
	register int c;
{
	static char buf[3];

	if (c == 0177)
		return ("^?");
	if (c >= 040) {
		buf[0] = c;
		buf[1] = 0;
	} else {
		buf[0] = '^';
		buf[1] = '@'+c;
		buf[2] = 0;
	}
	return (buf);
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

deadpeer()
{
	sigset(SIGPIPE, deadpeer);
	(void) mode(0);
	longjmp(peerdied, -1);
}

intr()
{
	sigset(SIGINT, intr);
	(void) mode(0);
	longjmp(toplevel, -1);
}

ttyflush(fd)
{
	int n;

	if ((n = tfrontp - tbackp) > 0)
		n = write(fd, tbackp, n);
	if (n < 0)
		return;
	tbackp += n;
	if (tbackp == tfrontp)
		tbackp = tfrontp = ttyobuf;
}

netflush(fd)
{
	int n;

	if ((n = nfrontp - nbackp) > 0)
		n = write(fd, nbackp, n);
	if (n < 0) {
		if (errno != ENOBUFS && errno != EWOULDBLOCK) {
			(void) mode(0);
			perror(hostname);
			close(fd);
			longjmp(peerdied, -1);
			/*NOTREACHED*/
		}
		n = 0;
	}
	nbackp += n;
	if (nbackp == nfrontp)
		nbackp = nfrontp = netobuf;
}

/*VARARGS*/
printoption(direction, fmt, option, what)
	char *direction, *fmt;
	int option, what;
{
	if (!showoptions)
		return;
	printf("%s ", direction);
	if (fmt == doopt)
		fmt = "do";
	else if (fmt == dont)
		fmt = "dont";
	else if (fmt == will)
		fmt = "will";
	else if (fmt == wont)
		fmt = "wont";
	else
		fmt = "???";
	if (option < TELOPT_SUPDUP)
		printf("%s %s", fmt, telopts[option]);
	else
		printf("%s %d", fmt, option);
	if (*direction == '<') {
		printf("\r\n");
		return;
	}
	printf(" (%s)\r\n", what ? "reply" : "don't reply");
}
