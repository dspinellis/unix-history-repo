#ifndef lint
static char *rcsid = "$Header: gaptelnet.c,v 2.2 86/05/16 11:03:33 jqj Exp $";
#endif

/*
 * XNS User telnet program.
 */

/* $Log:	gaptelnet.c,v $
 * Revision 2.2  86/05/16  11:03:33  jqj
 * fix to correspond to new semantics for enumerations (global)
 * 
 * Revision 2.1  86/03/01  09:26:04  jqj
 * Accept data with datastream=0 for the sake of incorrectly implemented
 * servers (e.g. InterLisp-D).  If unrecognized inband controls arrive,
 * don't choke.
 * 
 * Revision 2.0  85/11/21  07:23:04  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.3  85/11/20  14:00:08  jqj
 * added symbolic entries for Gap connection types
 * 
 * Revision 1.2  85/05/22  09:46:37  jqj
 * VAX 4.3beta baseline version
 * 
 * Revision 1.2  85/05/22  09:46:37  jqj
 * Beta-test GAP telnet
 * 
 * based on tcp/telnet:
 *	static char *rcsid = "$Header: gaptelnet.c,v 2.2 86/05/16 11:03:33 jqj Exp $";
 *	static char sccsid[] = "@(#)telnet.c	4.24 (Berkeley) 7/20/83";
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <netns/ns.h>
#include <netns/idp.h>
#include <netns/sp.h>		/* for spphdr */
#include <netns/spidp.h>

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>

#include <xnscourier/Clearinghouse2.h>
#include "GAP3.h"
#include "gapcontrols.h"
#include <xnscourier/except.h>
#include <xnscourier/CH.h>

#define	strip(x)	((x)&0177)

char	ttyobuf[BUFSIZ], *tfrontp = ttyobuf, *tbackp = ttyobuf;
char	netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;


int	connected;
CourierConnection *cconn;
int	net;
FILE	*logfile;
int	debug = 0;
int	crmod = 0;
char	*prompt;
char	escape = CTRL(]);
char	on = 1;

char	line[200];
int	margc;
char	*margv[20];

jmp_buf	toplevel;
jmp_buf	peerdied;

extern	int errno;

int	tn(), quit(), suspend(), bye(), help();
int	setescape(), status(), toggle(), setoptions();
int	setcrmod(), setdebug(), setlog();

#define HELPINDENT (sizeof ("connect"))

struct cmd {
	char	*name;		/* command name */
	char	*help;		/* help string */
	int	(*handler)();	/* routine which executes command */
};

char	openhelp[] =	"connect to a site";
char	closehelp[] =	"close current connection";
char	quithelp[] =	"exit telnet";
char	zhelp[] =	"suspend telnet";
char	debughelp[] =	"toggle debugging";
char	escapehelp[] =	"set escape character";
char	statushelp[] =	"print status information";
char	helphelp[] =	"print help information";
char	crmodhelp[] =	"toggle mapping of received carriage returns";
char	loghelp[] =	"toggle logging of session";

struct cmd cmdtab[] = {
	{ "open",	openhelp,	tn },
	{ "close",	closehelp,	bye },
	{ "quit",	quithelp,	quit },
	{ "z",		zhelp,		suspend },
	{ "escape",	escapehelp,	setescape },
	{ "status",	statushelp,	status },
/*	{ "crmod",	crmodhelp,	setcrmod },	*/
	{ "debug",	debughelp,	setdebug },
	{ "log",	loghelp,	setlog },
	{ "?",		helphelp,	help },
	0
};

struct sockaddr_ns sin;

int	intr(), deadpeer();
char	*control();
struct	cmd *getcmd();

struct	tchars otc;
struct	ltchars oltc;
struct	sgttyb ottyb;

char	*hostname;
char	hnamebuf[45];


main(argc, argv)
	int argc;
	char *argv[];
{
	ioctl(0, TIOCGETP, (char *)&ottyb);
	ioctl(0, TIOCGETC, (char *)&otc);
	ioctl(0, TIOCGLTC, (char *)&oltc);
	setbuf(stdin, 0);
	setbuf(stdout, 0);
	prompt = argv[0];
	if (argc > 1 && !strcmp(argv[1], "-d"))
		debug = SO_DEBUG, argv++, argc--;
	if (argc != 1) {
		if (setjmp(toplevel) != 0)
			exit(0);
		tn(argc, argv);
	}
	setjmp(toplevel);
	for (;;)
		command(1);
}

tn(argc, argv)
	int argc;
	char *argv[];
{
	register int c;
	register struct ns_addr *host;
	extern struct ns_addr *getXNSaddr();
	Clearinghouse2_ObjectName hostoname, hdefault;
	LongCardinal servicetype;

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
		printf("usage: %s host-name [service-type]\n", argv[0]);
		return;
	}
	if (argc == 2) servicetype = TTYService_sa;	/* default to 1 */
	else if (strcmp(argv[2],"sa") == 0) servicetype = TTYService_sa;
	else if (strncmp(argv[2],"re",2) == 0 ||
		 strcmp(argv[2],"exec") == 0) servicetype = TTYService_exec;
	else if (strcmp(argv[2],"its") == 0) servicetype = TTYService_its;
	else servicetype = atoi(argv[2]);
	CH_NameDefault(&hdefault);
	hostoname = CH_StringToName(argv[1], &hdefault);
	if ((host = CH_LookupAddrDN(hostoname,0,hnamebuf,sizeof(hnamebuf)))) {
		sin.sns_family = AF_NS;
		host->x_port = htons(IDPPORT_COURIER);
		bcopy(host, (caddr_t)&sin.sns_addr, sizeof(host));
		/* hnamebuf is filled in by CH_LookupAddrDN */
		hostname = hnamebuf;
	} else if ((host = getXNSaddr(argv[1]))) {
		sin.sns_family = AF_NS;
		bcopy(host, (caddr_t)&sin.sns_addr, sizeof(host));
		strcpy(hnamebuf, argv[1]);
		hostname = hnamebuf;
	} else {			
		printf("%s: unknown host\n", argv[1]);
		return;
	}
	cconn = CourierOpen(host);
	if(cconn == NULL) {
		fprintf(stderr,"Courier connection failed\n");
		return;
	}
	net = *(int*)cconn;
	signal(SIGINT, intr);
	signal(SIGPIPE, deadpeer);
	printf("Trying...\n");
	if (createsession(cconn,servicetype) < 0)
	  return;
	connected++;
	call(status, "status", 0);
	sleep(1);
	if (setjmp(peerdied) == 0)
		telnet(net);
	fprintf(stderr, "\nConnection closed by foreign host.\n");
	exit(1);
}

/*
 * create a session
 */
createsession(cconn, servicetype)
	CourierConnection *cconn;
	LongCardinal servicetype;
{
	GAP3_SessionParameterObject pobj;
	GAP3_TransportObject tobjs[2];
	GAP3_CommParamObject *cp;
	struct {
		Cardinal length;
		GAP3_TransportObject *sequence;
	} tobjlist;
	Authentication1_Credentials creds;
	Authentication1_Verifier verifier;

	pobj.designator = GAP3_oldTtyHost; /* 11 */
	pobj.GAP3_oldTtyHost_case.charLength = GAP3_seven;
	pobj.GAP3_oldTtyHost_case.parity = GAP3_none;
	pobj.GAP3_oldTtyHost_case.stopBits = GAP3_oneStopBit;
	pobj.GAP3_oldTtyHost_case.frameTimeout = 20;
/*
	tobjs[0].designator = GAP3_rs232c;
	cp = &tobjs[0].GAP3_rs232c_case.commParams;
	cp->accessDetail.designator = GAP3_directConn;
	cp->accessDetail.directConn_case.duplex = GAP3_fullduplex;
	cp->accessDetail.directConn_case.lineType = GAP3_asynchronous;
	cp->accessDetail.directConn_case.lineSpeed = GAP3_bps300;
	tobjs[0].rs232c_case.preemptOthers = GAP3_preemptInactive;
	tobjs[0].rs232c_case.preemptMe = GAP3_preemptInactive;
	tobjs[0].rs232c_case.phoneNumber = "";
	tobjs[0].rs232c_case.line.designator = GAP3_reserveNeeded;
	tobjs[0].rs232c_case.line.reserveNeeded_case.lineNumber = 1;
*/
	tobjs[0].designator = GAP3_service;
	tobjs[0].GAP3_service_case.id = servicetype; /* 1 == SA */

	tobjs[1].designator = GAP3_teletype;
	tobjlist.length = 2;
	tobjlist.sequence = tobjs;
	MakeSimpleCredsAndVerifier(0, 0, &creds, &verifier);
	DURING
	  (void) GAP3_Create(cconn, NULL, pobj, tobjlist, 0, creds, verifier);
	HANDLER {
		char *msg;
		switch (Exception.Code) {
		case GAP3_mediumConnectFailed:
			msg = "medium connect failed";
			break;
		case GAP3_illegalTransport:
			msg = "illegal transport type";
			break;
		case GAP3_tooManyGateStreams:
		case GAP3_serviceTooBusy:
			msg = "insufficient resources";
			break;
		case GAP3_serviceNotFound:
			msg = "service type not found";
			break;
		case GAP3_userNotAuthenticated:
		case GAP3_userNotAuthorized:
			msg = "authentication problem";
			break;
		case REJECT_ERROR:
			switch (CourierErrArgs(rejectionDetails,designator)){
			case noSuchProgramNumber:
				msg = "server does not support GAP";
				break;
			case noSuchVersionNumber:
				msg = "server does not support our GAP version";
				break;
			default:
				msg = "connection rejected";
			}
			break;
		case PROTOCOL_VIOLATION:
			msg = "protocol violation by remote server";
			break;
		default:
			msg = "some random error";
			break;
		}
		fprintf(stderr,"Error creating connection, %s\n",
			msg);
		return(-1);
	} END_HANDLER;
	return(0);
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
	fflush(stdout);
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
	ioctl(0, TIOCGETP, (char *)&ottyb);
	ioctl(0, TIOCGETC, (char *)&otc);
	ioctl(0, TIOCGLTC, (char *)&oltc);
	(void) mode(save);
}

/*VARARGS*/
bye()
{
	register char *op;

	(void) mode(0);
	if (connected) {
		sendoobdata(GAPCTLcleanup);
		setsockopt(net, NSPROTO_SPP, SO_HEADERS_ON_OUTPUT, &on,
			   sizeof(on));
		sppclose(net);
		printf("Connection closed.\n");
		connected = 0;
	}
}

/*VARARGS*/
quit()
{
	call(bye, "bye", 0);
	exit(0);
}

/*
 * Toggle debugging
 */
setdebug(argc, argv)
{
	debug = ~debug;
}

/*
 * Toggle logging
 */
setlog(argc, argv)
	int argc;
	char *argv[];
{
	if (argc > 2)
		printf("Syntax: %s [filename]\n",argv[0]);
	else if (logfile != (FILE*) 0) {
		/* currently logging */
		fclose(logfile);
		printf("Log file closed\n");
		logfile = (FILE*) 0;
		if (argc == 2 && (logfile = fopen(argv[1],"a")) != (FILE*)0)
			printf("Logging to %s\n",argv[1]);
	} else {
		/* not currently logging */
		if (argc == 1)
			printf("Logging already disabled\n");
		else if (argc == 2 && 
			 (logfile = fopen(argv[1],"a")) != (FILE*)0 )
			printf("Logging to %s\n",argv[1]);
	}
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

struct	tchars notc =	{ -1, -1, -1, -1, -1, -1 };
struct	ltchars noltc =	{ -1, -1, -1, -1, -1, -1 };

mode(f)
	register int f;
{
	static int prevmode = 0;
	struct tchars *tc;
	struct ltchars *ltc;
	struct sgttyb sb;
	int onoff, old;

	if (prevmode == f)
		return (f);
	old = prevmode;
	prevmode = f;
	sb = ottyb;
	switch (f) {

	case 0:
		onoff = 0;
		tc = &otc;
		ltc = &oltc;
		break;

	case 1:
	case 2:
		sb.sg_flags |= CBREAK;
		if (f == 1)
			sb.sg_flags &= ~(ECHO|CRMOD);
		else
			sb.sg_flags |= ECHO|CRMOD;
		sb.sg_erase = sb.sg_kill = -1;
		tc = &notc;
		ltc = &noltc;
		onoff = 1;
		break;

	default:
		return (old);
	}
	ioctl(fileno(stdin), TIOCSLTC, (char *)ltc);
	ioctl(fileno(stdin), TIOCSETC, (char *)tc);
	ioctl(fileno(stdin), TIOCSETP, (char *)&sb);
	ioctl(fileno(stdin), FIONBIO, &onoff);
	ioctl(fileno(stdout), FIONBIO, &onoff);
	return (old);
}

struct {struct sphdr hdr;
	char data[BUFSIZ];
} sibuf;
char	*sbp;
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
	int ibits, obits;

	(void) mode(1);
	ioctl(s, FIONBIO, &on);
	changeSPPopts(net, GAPCTLnone, 1); /* datastream "normal", eom */
	for (;;) {
		ibits = obits = 0;
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
			scc = read(s, &sibuf, sizeof (sibuf))
			  - sizeof(struct sphdr);
#ifdef DEBUG
			if (debug)
			  printf("reading %d bytes from net\n", scc);
#endif
			if (scc < 0 && errno == EWOULDBLOCK)
				scc = 0;
			else if (scc < 0)
				break; /* protocol violation? */
			else if (sibuf.hdr.sp_cc & SP_OB) {
				/* status or OOB control */
				switch ((u_char) *sibuf.data) {
				case GAPCTLareYouThere:
					sendoobdata(GAPCTLiAmHere);
					break;
				case GAPCTLmediumDown:
					(void) mode(0);
					longjmp(peerdied, -1);
					/*NOTREACHED*/
				default:
					/* ignore others */
					break;
				}
				scc = 0;
			}
			else if (sibuf.hdr.sp_dt == GAPCTLnone ||	
				 sibuf.hdr.sp_dt == 0) {
				/* normal case, plus Lisp bogosity */
				sbp = sibuf.data;
			}
			else if(sibuf.hdr.sp_dt == GAPCTLcleanup){
				sendoobdata(GAPCTLcleanup);
				/* should get an END next */
				scc = 0;
			}
			else if(sibuf.hdr.sp_dt == SPPSST_END) {
				setsockopt(net, NSPROTO_SPP, 
					SO_HEADERS_ON_OUTPUT,
					&on, sizeof(on));
				sppclosereply(net);
				(void) mode(0);
				longjmp(peerdied, -1);
				/*NOTREACHED*/
			}
			else scc = 0;	/* ignore other inband controls */
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
			/* We don't do any input translation at the moment */
#ifdef notdef
			switch (c) {
			case '\n':
				*nfrontp++ = '\r';
				*nfrontp++ = '\n';
				break;
			case '\r':
				*nfrontp++ = '\r';
				*nfrontp++ = '\n';
				break;
			default:
				*nfrontp++ = c;
				break;
			}
#else
			*nfrontp++ = c;
#endif /* notdef */
		}
		if ((obits & (1 << s)) && (nfrontp - nbackp) > 0)
			netflush(s);
		while (scc > 0) {
			register int c;
			c = *sbp++&0377; scc--;
			/* nor do we do any output translation */
			*tfrontp++ = c;
		}
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
	fflush(stdout);
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
	(void) mode(0);
	longjmp(peerdied, -1);
}

intr()
{
	(void) mode(0);
	longjmp(toplevel, -1);
}

ttyflush(fd)
{
	register int n;

	if ((n = tfrontp - tbackp) > 0) {
		if (logfile != (FILE*)0)
			fwrite(tbackp, 1, n, logfile);
		n = write(fd, tbackp, n);
	}
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
#ifdef DEBUG
	if (debug)
	  printf("writing %d of %d bytes to net\n", n, nfrontp-nbackp);
#endif
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

/*
 * Send out of band data to other end of network
 */
sendoobdata(value)
	char value;
{
	send(net, &value, 1, MSG_OOB);
}

changeSPPopts(s, stream, eom)
	int s;			/* SPP socket */
	u_char stream;		/* datastream type */
	char eom;		/* Boolean EOM */
{
	struct sphdr sphdr;
	int off = 0;

	sphdr.sp_dt = stream;
	sphdr.sp_cc = (eom ? SP_EM : 0);
	setsockopt(s, NSPROTO_SPP, SO_HEADERS_ON_OUTPUT, &off, sizeof(off));
	setsockopt(s, NSPROTO_SPP, SO_DEFAULT_HEADERS, &sphdr, sizeof(sphdr));
}
