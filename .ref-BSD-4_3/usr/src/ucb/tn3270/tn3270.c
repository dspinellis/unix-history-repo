/*
 *	Copyright 1984, 1985 by the Regents of the University of
 *	California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */


#ifndef lint
static char sccsid[] = "@(#)tn3270.c	2.7\t5/13/86";
#endif

/*
 * User telnet program, specially modified for tn3270.
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/time.h>

#include <netinet/in.h>

#define	TELOPTS
#include <arpa/telnet.h>

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#include <netdb.h>

#define	strip(x)	((x)&0177)
#define min(x,y)	((x<y)? x:y)

static char	Ibuf[8*BUFSIZ], *Ifrontp = Ibuf, *Ibackp = Ibuf;
static char	ttyobuf[BUFSIZ], *tfrontp = ttyobuf, *tbackp = ttyobuf;
static char	netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;

static char	SbBuffer[100], *pSb = SbBuffer;
#define Sb_Option SbBuffer[0]
#define Sb_Command SbBuffer[1]


static char	hisopts[256];
static char	myopts[256];

static char	doopt[] = { IAC, DO, '%', 'c', 0 };
static char	dont[] = { IAC, DONT, '%', 'c', 0 };
static char	will[] = { IAC, WILL, '%', 'c', 0 };
static char	wont[] = { IAC, WONT, '%', 'c', 0 };
static char	sb_terminal[] = { IAC, SB,
			TELOPT_TTYPE, TELQUAL_IS,
			'I', 'B', 'M', '-', '3', '2', '7', '7', '-', '2',
			IAC, SE };

/* The following is a real, live, global. */

/* The point of HaveInput is to give a hint to the terminal output processor
 * that some input from some source (network or terminal) has come in.
 */

int HaveInput = 1;	/* we have received input in the not too distant past */


static int	connected;
static int	SentTerminalType = 0;		/* returned sb_terminal to other? */
static int	In3270 = 0;			/* we are in 3270 binary mode */
static int	ISend = 0;			/* trying to send network data in */
static int	ForceMode = -1;			/* for debugging */
static int	net;
static int	showoptions = 0;
static int	debug = 0;
static int	crmod = 0;
static int	printnet = 0;
static FILE	*NetTrace;
static char	*prompt;
static char	escape = CTRL(]);

static char	line[200];
static int	margc;
static char	*margv[20];

static jmp_buf	toplevel;
static jmp_buf	peerdied;

extern	int errno;

int	quit(), suspend();
static int	tn(), bye(), help();
static int	setescape(), status(), toggle(), setoptions();
static int	setcrmod(), setdebug(), SetPrintNet();

#define HELPINDENT (sizeof ("connect"))

struct cmd {
	char	*name;		/* command name */
	char	*help;		/* help string */
	int	(*handler)();	/* routine which executes command */
	int	dohelp;		/* Should we give general help information? */
};

static char	openhelp[] =	"connect to a site";
static char	closehelp[] =	"close current connection";
static char	quithelp[] =	"exit telnet";
static char	zhelp[] =	"suspend telnet";
static char	debughelp[] =	"toggle debugging";
static char	escapehelp[] =	"set escape character";
static char	statushelp[] =	"print status information";
static char	helphelp[] =	"print help information";
static char	optionshelp[] =	"toggle viewing of options processing";
static char	crmodhelp[] =	"toggle mapping of received carriage returns";
static char	printnethelp[] = "print out raw data to/from net";

static struct cmd cmdtab[] = {
	{ "open",	openhelp,	tn, 1 },
	{ "close",	closehelp,	bye, 1 },
	{ "quit",	quithelp,	quit, 1 },
	{ "z",		zhelp,		suspend, 1 },
	{ "suspend",	zhelp,		suspend, 0 },
	{ "escape",	escapehelp,	setescape, 0 },
	{ "status",	statushelp,	status, 1 },
	{ "options",	optionshelp,	setoptions, 0 },
	{ "crmod",	crmodhelp,	setcrmod, 0 },
	{ "debug",	debughelp,	setdebug, 0 },
	{ "printnet",	printnethelp,	SetPrintNet, 0 },
	{ "?",		helphelp,	help, 1 },
	{ "help",	helphelp,	help, 0 },
	0
};

static struct sockaddr_in sin;

static int	intr(), deadpeer(), inputAvailable();
static char	*control();
static struct	cmd *getcmd();
static struct	servent *sp;

static struct	tchars otc;
static struct	ltchars oltc;
static struct	sgttyb ottyb;

main(argc, argv)
	int argc;
	char *argv[];
{
	ioctl(0, TIOCGETP, (char *)&ottyb);
	ioctl(0, TIOCGETC, (char *)&otc);
	ioctl(0, TIOCGLTC, (char *)&oltc);
	sp = getservbyname("telnet", "tcp");
	if (sp == 0) {
		ExitString(stderr, "telnet: tcp/telnet: unknown service\n", 1);
	}
	NetTrace = stdout;
	prompt = argv[0];
	if (argc > 1 && !strcmp(argv[1], "-d")) {
		debug = SO_DEBUG, argv++, argc--;
	}
	if (argc > 1 && !strcmp(argv[1], "-n")) {
	    argv++;
	    argc--;
	    if (argc > 1) {		/* get file name */
		NetTrace = fopen(argv[1], "w");
		argv++;
		argc--;
		if (NetTrace == NULL) {
		    NetTrace = stdout;
		}
	    }
	}
	if (argc != 1) {
		if (setjmp(toplevel) != 0)
			Exit(0);
		tn(argc, argv);
	}
	setjmp(toplevel);
	for (;;)
		command(1);
}

static char	*hostname;
static char	hnamebuf[32];

static
tn(argc, argv)
	int argc;
	char *argv[];
{
	register struct hostent *host;
	char *strcpy();

	if (connected) {
		printf("?Already connected to %s\n", hostname);
		return;
	}
	if (argc < 2) {
		(void) strcpy(line, "Connect ");
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
		(void) strcpy(hnamebuf, argv[1]);
		hostname = hnamebuf;
	}
	sin.sin_port = sp->s_port;
	if (argc == 3) {
		sin.sin_port = atoi(argv[2]);
		sin.sin_port = htons(sin.sin_port);
	}
	net = socket(AF_INET, SOCK_STREAM, 0);
	if (net < 0) {
		perror("telnet: socket");
		return;
	}
	if (debug && setsockopt(net, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
		perror("setsockopt (SO_DEBUG)");
	signal(SIGINT, intr);
	signal(SIGPIPE, deadpeer);
	printf("Trying...\n");
	if (connect(net, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
		perror("telnet: connect");
		signal(SIGINT, SIG_DFL);
		return;
	}
	connected++;
	call(status, "status", 0);
	if (setjmp(peerdied) == 0)
		telnet();
	if (In3270) {
	    Stop3270(1);
	}
	ExitString(stderr, "Connection closed by foreign host.\n", 1);
}

/*
 * Print status about the connection.
 */
/*VARARGS*/
static
status()
{
	if (connected)
		printf("Connected to %s.\n", hostname);
	else
		printf("No connection.\n");
	/*printf("Escape character is '%s'.\n", control(escape));*/
	fflush(stdout);
}

static
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
	ioctl(0, TIOCGETP, (char *)&ottyb);
	ioctl(0, TIOCGETC, (char *)&otc);
	ioctl(0, TIOCGLTC, (char *)&oltc);
	(void) mode(save);
}

/*VARARGS*/
static
bye()
{
	register char *op;

	(void) mode(0);
	if (connected) {
		shutdown(net, 2);
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
	Exit(0);
}

/*
 * Help command.
 */
static
help(argc, argv)
	int argc;
	char *argv[];
{
	register struct cmd *c;

	if (argc == 1) {
		printf("Commands may be abbreviated.  Commands are:\n\n");
		for (c = cmdtab; c->name; c++)
			if (c->dohelp) {
			    printf("%-*s\t%s\n", HELPINDENT, c->name, c->help);
			}
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
static
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

static struct	tchars notc =	{ -1, -1, -1, -1, -1, -1 };
static struct	ltchars noltc =	{ -1, -1, -1, -1, -1, -1 };

mode(f)
	register int f;
{
	static int prevmode = 0;
	struct tchars *tc, tc3;
	struct ltchars *ltc;
	struct sgttyb sb;
	int onoff, old;

	if (prevmode == f)
		return (f);
	old = prevmode;
	prevmode = f;
	sb = ottyb;
	if (ForceMode != -1) {
		f = ForceMode;
		ForceMode = -1;
	}
	switch (f) {

	case 0:
		onoff = 0;
		tc = &otc;
		ltc = &oltc;
		break;

	case 1:		/* the rawest */
	case 2:		/* allows for local echoing, newline mapping */
	case 3:		/* like 1, but with XON/XOFF */

		sb.sg_flags |= CBREAK;
		if ((f == 1) || (f == 3)) {
		    sb.sg_flags &= ~(ECHO|CRMOD);
		} else {
		    sb.sg_flags |= ECHO|CRMOD;
		}
		sb.sg_erase = sb.sg_kill = -1;
		if (f == 3) {
		    tc = &tc3;
		    tc3 = notc;
			/* get XON, XOFF characters */
		    tc3.t_startc = otc.t_startc;
		    tc3.t_stopc = otc.t_stopc;
		} else {
		    tc = &notc;
		}
		ltc = &noltc;
		onoff = 1;
		break;

	default:
		return(old);
	}
	ioctl(fileno(stdin), TIOCSLTC, (char *)ltc);
	ioctl(fileno(stdin), TIOCSETC, (char *)tc);
	ioctl(fileno(stdin), TIOCSETP, (char *)&sb);
	ioctl(fileno(stdin), FIONBIO, (char *)&onoff);
	ioctl(fileno(stdout), FIONBIO, (char *)&onoff);
	ioctl(fileno(stdin), FIOASYNC, (char *)&onoff);
	return (old);
}

static char	sibuf[BUFSIZ], *sbp;
static char	tibuf[BUFSIZ], *tbp;
static int	scc, tcc;
static int tin, tout;			/* file descriptors */

/*
 * Select from tty and network...
 */
static
telnet()
{
    int on = 1;
    int negativePid = -getpid();
    int schedValue;

    (void) mode(2);
    ioctl(net, FIONBIO, (char *)&on);
    ioctl(net, FIOASYNC, (char *)&on);	/* hear about input */
    ioctl(net, SIOCSPGRP, (char *)&negativePid);	/* set my pid */
    tin = fileno(stdin);
    tout = fileno(stdout);

    for (;;) {
	while (schedValue = Scheduler(0)) {
	    if (schedValue == -1) {
		(void) mode(0);
		return;
	    }
	}
		/* If there is data waiting to go out to terminal, don't
		 * schedule any more data for the terminal.
		 */
	if (tfrontp-tbackp) {
	    schedValue = 1;
	} else {
	    schedValue = DoTerminalOutput();
	}
	if (schedValue) {
	    if (Scheduler(1) == -1) {
		(void) mode(0);
		return;
	    }
	}
    }
}


/* Loop around once. */

static
Scheduler(block)
int block;		/* should we block in the select? */
{
    register int c;
    int ibits = 0, obits = 0;
		/* One wants to be a bit careful about setting returnValue
		 * to one, since a one implies we did some useful work,
		 * and therefore probably won't be called to block next
		 * time.
		 */
    int returnValue = 0;
    static struct timeval TimeValue = {0};

    if (!In3270) {
	if (nfrontp - nbackp)
		obits |= (1 << net);
	else if (tcc == 0) {
		ibits |= (1 << tin);
	}
	if (tfrontp - tbackp)
		obits |= (1 << tout);
	else if (!ISend)
		ibits |= (1 << net);
    } else {
	if (nfrontp - nbackp) {	/* something for network? */
	    obits |= 1<<net;		/* yes - wait for space */
	}
	if (tcc == 0) {		/* any pending tty input? */
	    ibits |= 1<<tin;	/* no, look for new input */
	}
	if (tfrontp-tbackp) {	/* any pending tty output? */
	    obits |= 1<<tout;	/* yes - wait for space */
	}
	if (!ISend) {		/* any pending net input? */
	    ibits |= 1<<net;		/* no, look for new input */
	}
    }
    if (scc < 0 && tcc < 0) {
	    return(-1);
    }
    if (HaveInput) {		/* Reprime SIGIO handler if appropriate */
	HaveInput = 0;
	signal(SIGIO, inputAvailable);
    }
    select(16, &ibits, &obits, (int *) 0,
				(block)? (struct timeval *)0:&TimeValue);
    if (ibits == 0 && obits == 0 && block) {
		    /* I don't like this, does it ever happen? */
	    printf("sleep(5) from tn3270, after select\n");
	    sleep(5);
	    return(0);
    }

    /*
     * Something to read from the network...
     */
    if (ibits & (1 << net)) {
	    scc = read(net, sibuf, sizeof (sibuf));
	    if (scc < 0 && errno == EWOULDBLOCK)
		    scc = 0;
	    else {
		    if (scc <= 0)
			    return(-1);
		    sbp = sibuf;
		    if (printnet) {
			    Dump('<', sbp, scc);
		    }
		    returnValue = 1;		/* did something usefull */
	    }
    }

    /*
     * Something to read from the tty...
     */
    if (ibits & (1 << tin)) {
	    tcc = read(tin, tibuf, sizeof tibuf);
	    if (tcc < 0 && errno == EWOULDBLOCK)
		    tcc = 0;
	    else {
		    if (tcc <= 0)
			    return(-1);
		    tbp = tibuf;
		    returnValue = 1;		/* did something usefull */
	    }
    }

    if (tcc > 0) {
	if (In3270) {
	    c = DataFromTerminal(tbp, tcc);
	    if (c) {
		returnValue = 1;		/* did something usefull */
	    }
	    tcc -= c;
	    tbp += c;
	} else {
	    returnValue = 1;		/* did something usefull */
	    while (tcc > 0) {
		if ((&netobuf[BUFSIZ] - nfrontp) < 2)
			break;
		c = *tbp++ & 0377, tcc--;
		if (strip(c) == escape) {
			command(0);
			tcc = 0;
			break;
		}
		if (c == IAC)
			*nfrontp++ = c;
		*nfrontp++ = c;
	    }
	}
    }
    if ((obits & (1 << net)) && (c = (int) (nfrontp - nbackp)) > 0) {
	netflush();
	if (c != (int) (nfrontp-nbackp)) {
	    returnValue = 1;
	}
    }
    if (scc > 0) {
	if (Ifrontp+scc >= Ibuf+sizeof Ibuf) {
	    if (Ibackp != Ibuf) {	/* do some copying */
		bcopy(Ibackp, Ibuf, Ifrontp-Ibackp);
		Ifrontp -= (Ibackp-Ibuf);
		Ibackp = Ibuf;
	    }
	}
	if (Ifrontp+scc < Ibuf+sizeof Ibuf) {
	    returnValue = 1;		/* doing something useful */
	    telrcv();
	}		/* Else - we may never recover */
    }
    if ((obits & (1 << tout)) && (c = (int) (tfrontp - tbackp)) > 0) {
	ttyflush();
	if (c != (int) (tfrontp-tbackp)) {
	    returnValue = 1;
	}
    }
    if (In3270 && (c = (int) (Ifrontp-Ibackp))) {
	    Ibackp += DataFromNetwork(Ibackp, Ifrontp-Ibackp, ISend);
	    if (c != (int) (Ifrontp-Ibackp)) {
		returnValue = 1;
	    }
	    if (Ibackp == Ifrontp) {
		    Ibackp = Ifrontp = Ibuf;
		    ISend = 0;	/* take data from network */
	    }
    }
    return(returnValue);				/* good return */
}

command(top)
	int top;
{
	register struct cmd *c;
	int oldmode;

	oldmode = mode(0);
	if (!top)
		putchar('\n');
	else
		signal(SIGINT, SIG_DFL);
	for (;;) {
		printf("%s> ", prompt);
		if (gets(line) == 0) {
			if (feof(stdin)) {
				clearerr(stdin);
				putchar('\n');
			}
			break;
		}
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
#define TS_SB		6	/* in sub-negotiation */
#define	TS_SE		7	/* coming out of sub-negotiation */

#define SB_ACCUM(c)	{*pSb = c;	/* accumulate character */ \
			if (pSb >= SbBuffer+sizeof (SbBuffer)) { \
				/* can't accept any more */ \
				pSb = SbBuffer; \
			} \
			pSb++;}

static
telrcv()
{
	register int c;
	register char *Sbp;
	register int Scc;
	static int state = TS_DATA;

	while (scc > 0) {
		c = *sbp++ & 0377, scc--;
		switch (state) {

		case TS_DATA:
			if (c == IAC) {
			    state = TS_IAC;
			    continue;
			}
				/* We optimize this loop, since it is
				 * where we spend 99% of this routine.
				 */
			if (In3270) {
			    *Ifrontp++ = c;
			    Sbp = sbp;
			    Scc = scc;
			    while (Scc > 0) {
				c = *Sbp++ & 0377, Scc--;
				if (c == IAC) {
				    state = TS_IAC;
				    break;
				}
				*Ifrontp++ = c;
			    }
			    sbp = Sbp;
			    scc = Scc;
			} else {
			    *tfrontp++ = c;
			    /*
			     * This hack is needed since we can't set
			     * CRMOD on output only.  Machines like MULTICS
			     * like to send \r without \n; since we must
			     * turn off CRMOD to get proper input, the mapping
			     * is done here (sigh).
			     */
			    if (c == '\r' && crmod && !In3270)
				    *tfrontp++ = '\n';
			}
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
				outputPurge();
				break;

			case NOP:
			case GA:
				break;

			case SB:
				state = TS_SB;
				pSb = SbBuffer;	/* where to collect */
				continue;

			case EOR:
				if (In3270) {
				    Ibackp += DataFromNetwork(Ibackp,
						Ifrontp-Ibackp, 1);
				    if (Ibackp == Ifrontp) {
					Ibackp = Ifrontp = Ibuf;
					ISend = 0;	/* should have been! */
				    } else {
					ISend = 1;
				    }
				}
				break;

			case IAC:
				if (In3270) {
				    *Ifrontp++ = IAC;
				} else {
				    *tfrontp++ = IAC;
				}
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
				if (c == TELOPT_BINARY) {
				    SetIn3270();
				}
				sprintf(nfrontp, wont, c);
				nfrontp += sizeof (wont) - 2;
				printoption("SENT", wont, c);
			}
			state = TS_DATA;
			continue;
		case TS_SB:
			if (c == IAC) {
				state = TS_SE;
				continue;
			}
			SB_ACCUM(c);
			continue;
		case TS_SE:
		    if (c != SE) {
			if (c != IAC) {
			    SB_ACCUM(IAC);
			}
			SB_ACCUM(c);
			state = TS_SB;
		    } else {
			/* this is the end of the sub negotiation */
			/* we only allow a termtype, send, sub */
			if ((Sb_Option != TELOPT_TTYPE) ||
				(Sb_Command != TELQUAL_SEND)) {
			    /* what to do? XXX */
			} else {
			    /* send our type */
			    SentTerminalType = 1;
			    SetIn3270();
			    bcopy(sb_terminal, nfrontp, sizeof sb_terminal);
			    nfrontp += sizeof sb_terminal;
			    printoption("SENT", sb_terminal,
					TELOPT_TTYPE);
			}
			state = TS_DATA;
		    }
		}
	}
}

static
willoption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_ECHO:
		(void) mode(1);

	case TELOPT_BINARY:
		hisopts[option] = 1;
		SetIn3270();
		fmt = doopt;
		break;

	case TELOPT_EOR:
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

static
wontoption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_BINARY:
		hisopts[option] = 0;
		SetIn3270();
		fmt = doopt;
		break;

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

static
dooption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_TTYPE:
	case TELOPT_BINARY:
		myopts[option] = 1;
		SetIn3270();
		fmt = will;
		break;

	case TELOPT_TM:
		fmt = wont;
		break;

	case TELOPT_ECHO:
		(void) mode(2);
		fmt = will;
		hisopts[option] = 0;
		break;

	case TELOPT_EOR:
	case TELOPT_SGA:
		fmt = will;
		break;

	default:
		fmt = wont;
		break;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += (sizeof dont)-2;
	printoption("SENT", fmt, option);
}

static
SetIn3270()
{
    if (SentTerminalType && myopts[TELOPT_BINARY] && hisopts[TELOPT_BINARY]) {
	if (!In3270) {
	    In3270 = 1;
	    OptInit();		/* initialize mappings */
	    /* initialize terminal key mapping */
	    (void) DataFromTerminal(ttyobuf, 0);
	    (void) mode(3);
	}
    } else {
	if (In3270) {
	    Stop3270(1);
	    In3270 = 0;
	    (void) mode(2);
	}
    }
}

/*
 * Set the escape character.
 */
static
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
	fflush(stdout);
}

/*VARARGS*/
static
setoptions()
{

	showoptions = !showoptions;
	printf("%s show option processing.\n", showoptions ? "Will" : "Wont");
	fflush(stdout);
}

/*VARARGS*/
static
setcrmod()
{

	crmod = !crmod;
	printf("%s map carriage return on output.\n", crmod ? "Will" : "Wont");
	fflush(stdout);
}

/*VARARGS*/
static
setdebug()
{

	debug = !debug;
	printf("%s turn on socket level debugging.\n",
		debug ? "Will" : "Wont");
	fflush(stdout);
	if (debug && net > 0 && setsockopt(net, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
		perror("setsockopt (SO_DEBUG)");
}

/*VARARGS*/
static
SetPrintNet()
{

	printnet = !printnet;
	printf("%s turn on printing of raw network traffic.\n",
		printnet ? "Will" : "Wont");
}

/*
 * Construct a control character sequence
 * for a special character.
 */
static char *
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

static struct cmd *
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

static
deadpeer()
{
	(void) mode(0);
	longjmp(peerdied, -1);
}

static
intr()
{
	(void) mode(0);
	longjmp(toplevel, -1);
}

static
inputAvailable()
{
    HaveInput = 1;
}

/* outputPurge() - get rid of all output destined for terminal. */
outputPurge()
{
    ioctl(fileno(stdout), TIOCFLUSH, (char *)0);
    tbackp = tfrontp = ttyobuf;
}

ttyflush()
{
	int n;

	if ((n = tfrontp - tbackp) > 0)
		n = write(tout, tbackp, n);
	if (n < 0)
		return;
	tbackp += n;
	if (tbackp == tfrontp)
		tbackp = tfrontp = ttyobuf;
}

/* TtyChars() - returns the number of characters in the TTY buffer */
TtyChars()
{
    return(tfrontp-tbackp);
}

netflush()
{
	int n;

	if ((n = nfrontp - nbackp) > 0)
		n = write(net, nbackp, n);
	if (n < 0) {
		if (errno != ENOBUFS && errno != EWOULDBLOCK) {
			(void) mode(0);
			perror(hostname);
			close(net);
			longjmp(peerdied, -1);
			/*NOTREACHED*/
		}
		n = 0;
	}
	if (printnet) {
		Dump('>', nbackp, n);
	}
	nbackp += n;
	if (nbackp == nfrontp)
		nbackp = nfrontp = netobuf;
}

/* DataToNetwork - queue up some data to go to network.  When last byte is
    queued, we add on an IAC EOR sequence (so, don't call us until you
    want that done...)
 */

int
DataToNetwork(buffer, count)
register char	*buffer;		/* where the data is */
register int	count;			/* how much to send */
{
    register int c;
    int origCount;

    origCount = count;

    while (count) {
	if ((&netobuf[sizeof netobuf] - nfrontp) < 6) {
	    netflush();
	    if ((&netobuf[sizeof netobuf] - nfrontp) < 6) {
		break;
	    }
	}
	c = *buffer++;
	count--;
	if (c == IAC) {
	    *nfrontp++ = IAC;
	    *nfrontp++ = IAC;
	} else {
	    *nfrontp++ = c;
	}
    }

    if (!count) {
	*nfrontp++ = IAC;
	*nfrontp++ = EOR;
	netflush();		/* try to move along as quickly as ... */
    }
    return(origCount - count);
}

/* DataToTerminal - queue up some data to go to terminal. */

int
DataToTerminal(buffer, count)
register char	*buffer;		/* where the data is */
register int	count;			/* how much to send */
{
    int origCount;
    int o;

    origCount = count;

    while (count) {
	if (tfrontp >= &ttyobuf[sizeof ttyobuf]) {
	    ttyflush();
	    while (tfrontp >= &ttyobuf[sizeof ttyobuf]) {
		o = 1<<tout;
		(void) select(tout+1, (int *) 0, &o, (int *) 0,
						(struct timeval *) 0);
		ttyflush();
	    }
	}
	*tfrontp++ = *buffer++;
	count--;
    }
    return(origCount - count);
}

/* EmptyTerminal - called to make sure that the terminal buffer is empty.
 *			Note that we consider the buffer to run all the
 *			way to the kernel (thus the select).
 */

void
EmptyTerminal()
{
    int o;

    o = 1<<tout;

    if (tfrontp == tbackp) {
	(void) select(tout+1, (int *) 0, &o, (int *) 0,
			(struct timeval *) 0);	/* wait for TTLOWAT */
    } else {
	while (tfrontp != tbackp) {
	    ttyflush();
	    (void) select(tout+1, (int *) 0, &o, (int *) 0,
				(struct timeval *) 0);	/* wait for TTLOWAT */
	}
    }
}



/* StringToTerminal - output a null terminated string to the terminal */

int
StringToTerminal(s)
char *s;
{
    int count;

    count = strlen(s);
    if (count) {
	(void) DataToTerminal(s, count);	/* we know it always goes... */
    }
}


/* _putchar - output a single character to the terminal.  This name is so that
 *	curses(3x) can call us to send out data.
 */

_putchar(c)
char c;
{
    if (tfrontp >= &ttyobuf[sizeof ttyobuf]) {
	(void) DataToTerminal(&c, 1);
    } else {
	*tfrontp++ = c;		/* optimize if possible. */
    }
}

static
SetForExit()
{
    (void) mode(2);			/* switch modes to flush output */
    (void) mode(0);
    fflush(stdout);
    fflush(stderr);
    if (In3270) {
	Stop3270(0);
    }
    (void) mode(2);			/* make sure we go back to mode 0 */
    (void) mode(0);
}

static
Exit(returnCode)
int returnCode;
{
    SetForExit();
    exit(returnCode);
}

ExitString(file, string, returnCode)
FILE *file;
char *string;
int returnCode;
{
    SetForExit();
    fwrite(string, 1, strlen(string), file);
    exit(returnCode);
}

ExitPerror(string, returnCode)
char *string;
int returnCode;
{
    SetForExit();
    perror(string);
    exit(returnCode);
}


static
Dump(direction, buffer, length)
char	direction;
char	*buffer;
int	length;
{
#   define BYTES_PER_LINE	32
    char *pThis;
    int offset;

    offset = 0;

    while (length) {
	/* print one line */
	fprintf(NetTrace, "%c 0x%x\t", direction, offset);
	pThis = buffer;
	buffer = buffer+min(length, BYTES_PER_LINE);
	while (pThis < buffer) {
	    fprintf(NetTrace, "%.2x", (*pThis)&0xff);
	    pThis++;
	}
	fprintf(NetTrace, "\n");
	length -= BYTES_PER_LINE;
	offset += BYTES_PER_LINE;
	if (length < 0) {
	    return;
	}
	/* find next unique line */
    }
}



/*VARARGS*/
static
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
	else if (fmt == sb_terminal)
		fmt = "will (terminal)";
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
