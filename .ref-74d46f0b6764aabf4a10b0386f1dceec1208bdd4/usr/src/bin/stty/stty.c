/*-
 * Copyright (c) 1980, 1989, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1989, 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)stty.c	5.20 (Berkeley) %G%";
#endif /* not lint */

/*
 * set teletype modes
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/syslog.h>
#define KERNEL
#include <sys/tty.h>
#undef KERNEL
#include <sys/termios.h>
#include <sys/file.h>
#include <errno.h>
#include <ctype.h>
#include <stdio.h>

#define eq(s1, s2)	(strcmp((s1), (s2)) == 0)
#define WRAPCOL 65

struct modes {
	char *name;
	long set;
	long unset;
};

struct modes imodes[] = {
	"ignbrk",	IGNBRK, 0,
	"-ignbrk",	0, IGNBRK,
	"brkint",	BRKINT, 0,
	"-brkint",	0, BRKINT,
	"ignpar",	IGNPAR, 0,
	"-ignpar",	0, IGNPAR,
	"parmrk",	PARMRK, 0,
	"-parmrk",	0, PARMRK,
	"inpck",	INPCK, 0,
	"-inpck",	0, INPCK,
	"istrip",	ISTRIP, 0,
	"-istrip",	0, ISTRIP,
	"inlcr",	INLCR, 0,
	"-inlcr",	0, INLCR,
	"igncr",	IGNCR, 0,
	"-igncr",	0, IGNCR,
	"icrnl",	ICRNL, 0,
	"-icrnl",	0, ICRNL,
	"ixon",		IXON, 0,
	"-ixon",	0, IXON,
	"flow",		IXON, 0,
	"-flow",	0, IXON,
	"ixoff",	IXOFF, 0,
	"-ixoff",	0, IXOFF,
	"tandem",	IXOFF, 0,
	"-tandem",	0, IXOFF,
	"ixany",	IXANY, 0,
	"-ixany",	0, IXANY,
	"decctlq",	0, IXANY,
	"-decctlq",	IXANY, 0,
	"imaxbel",	IMAXBEL, 0,
	"-imaxbel",	0, IMAXBEL,
	0
};

struct modes omodes[] = {
	"opost",	OPOST, 0,
	"-opost",	0, OPOST,
	"-litout",	OPOST, 0,
	"litout",	0, OPOST,
	"onlcr",	ONLCR, 0,
	"-onlcr",	0, ONLCR,
	"tabs",		0, OXTABS,	/* "preserve" tabs */
	"-tabs",	OXTABS, 0,
	"xtabs",	OXTABS, 0,
	"-xtabs",	0, OXTABS,
	"oxtabs",	OXTABS, 0,
	"-oxtabs",	0, OXTABS,
	0
};

struct modes cmodes[] = {
	"cs5",		CS5, CSIZE,
	"cs6",		CS6, CSIZE,
	"cs7",		CS7, CSIZE,
	"cs8",		CS8, CSIZE,
	"cstopb",	CSTOPB, 0,
	"-cstopb",	0, CSTOPB,
	"cread",	CREAD, 0,
	"-cread",	0, CREAD,
	"parenb",	PARENB, 0,
	"-parenb",	0, PARENB,
	"parodd",	PARODD, 0,
	"-parodd",	0, PARODD,
	"parity",	PARENB | CS7, PARODD | CSIZE,
	"evenp",	PARENB | CS7, PARODD | CSIZE,
	"oddp",		PARENB | CS7 | PARODD, CSIZE,
	"-parity",	CS8, PARODD | PARENB | CSIZE,
	"pass8",	CS8, PARODD | PARENB | CSIZE,
	"-evenp",	CS8, PARODD | PARENB | CSIZE,
	"-oddp",	CS8, PARODD | PARENB | CSIZE,
	"hupcl",	HUPCL, 0,
	"-hupcl",	0, HUPCL,
	"hup",		HUPCL, 0,
	"-hup",		0, HUPCL,
	"clocal",	CLOCAL, 0,
	"-clocal",	0, CLOCAL,
	"crtscts",	CRTSCTS, 0,
	"-crtscts",	0, CRTSCTS,
	0
};

struct modes lmodes[] = {
	"echo",		ECHO, 0,
	"-echo",	0, ECHO,
	"echoe",	ECHOE, 0,
	"-echoe",	0, ECHOE,
	"crterase",	ECHOE, 0,
	"-crterase",	0, ECHOE,
	"crtbs",	ECHOE, 0,   /* crtbs not supported, close enough */
	"-crtbs",	0, ECHOE,
	"echok",	ECHOK, 0,
	"-echok",	0, ECHOK,
	"echoke",	ECHOKE, 0,
	"-echoke",	0, ECHOKE,
	"crtkill",	ECHOKE, 0,
	"-crtkill",	0, ECHOKE,
	"altwerase",	ALTWERASE, 0,
	"-altwerase",	0, ALTWERASE,
	"iexten",	IEXTEN, 0,
	"-iexten",	0, IEXTEN,
	"echonl",	ECHONL, 0,
	"-echonl",	0, ECHONL,
	"echoctl",	ECHOCTL, 0,
	"-echoctl",	0, ECHOCTL,
	"ctlecho",	ECHOCTL, 0,
	"-ctlecho",	0, ECHOCTL,
	"echoprt",	ECHOPRT, 0,
	"-echoprt",	0, ECHOPRT,
	"prterase",	ECHOPRT, 0,
	"-prterase",	0, ECHOPRT,
	"isig",		ISIG, 0,
	"-isig",	0, ISIG,
	"icanon",	ICANON, 0,
	"-icanon",	0, ICANON,
	"noflsh",	NOFLSH, 0,
	"-noflsh",	0, NOFLSH,
	"tostop",	TOSTOP, 0,
	"-tostop",	0, TOSTOP,
	"mdmbuf",	MDMBUF, 0,
	"-mdmbuf",	0, MDMBUF,
	"flusho",	FLUSHO, 0,
	"-flusho",	0, FLUSHO,
	"pendin",	PENDIN, 0,
	"-pendin",	0, PENDIN,
	"crt",		ECHOE|ECHOKE|ECHOCTL, ECHOK|ECHOPRT,
	"-crt",		ECHOK, ECHOE|ECHOKE|ECHOCTL,
	"newcrt",	ECHOE|ECHOKE|ECHOCTL, ECHOK|ECHOPRT,
	"-newcrt",	ECHOK, ECHOE|ECHOKE|ECHOCTL, 
	"nokerninfo",	NOKERNINFO, 0,
	"-nokerninfo",	0, NOKERNINFO,
	"kerninfo",	0, NOKERNINFO,
	"-kerninfo",	NOKERNINFO, 0,
	0
};

/*
 * Special control characters.
 *
 * Each entry has a list of names.  The first is the primary name
 * and is used when printing the control character in the "name = val;"
 * form.  The second is an abbreviation which is guaranteed to be less
 * than or equal to four characters in length and is primarily used
 * when printing the values in columunar form (guarantees all will
 * fit within 80 cols).  The rest are optional aliases.
 * All names are recognized on the command line.
 */
#define MAXNAMES 3
struct {
	char	*names[MAXNAMES+1];
	int	sub;
	u_char	def;
} cchars[] = {
	{{ "erase", "era" },		VERASE,	CERASE, },
	{{ "werase", "wera" },		VWERASE, CWERASE, },
	{{ "kill", "kill" },		VKILL,	CKILL, },
	{{ "intr", "int" },		VINTR,	CINTR, },
	{{ "quit", "quit" },		VQUIT,	CQUIT, },
	{{ "susp", "susp" },		VSUSP,	CSUSP, },
	{{ "dsusp", "dsus" },		VDSUSP,	CDSUSP, },
	{{ "eof", "eof" },		VEOF,	CEOF, },
	{{ "eol", "eol", "brk" },	VEOL,	CEOL, },
	{{ "eol2", "eol2" },		VEOL2,	CEOL, },
	{{ "stop", "stop", "xoff" },	VSTOP,	CSTOP, },
	{{ "start", "star", "xon" },	VSTART,	CSTART, },
	{{ "lnext", "lnxt" },		VLNEXT,	CLNEXT, },
	{{ "discard", "disc", "flush" },	VDISCARD, CDISCARD, },
	{{ "reprint", "rpnt", "rprnt" },	VREPRINT, CREPRINT, },
	{{ "status", "stat" },		VSTATUS, CSTATUS, },
	0
};

struct winsize win;
int ldisc;
int debug = 0;
int trace, dotrace;
int extproc;

#define OUT	stdout		/* informational output stream */
#define ERR	stderr		/* error message stream */
#define CTL	0		/* default control descriptor */
int ctl = CTL;

extern errno;

#define NORMAL	0	/* only print modes differing from defaults */
#define ALL	1	/* print all modes - POSIX standard format */
#define ALL_BSD	2	/* print all modes - using BSD shorthand for cc's */
#define	GFMT	3	/* print modes in form suitable to be re-input */

main(argc, argv) 
	char *argv[];
{
	struct termios t;
	int i, fmt = NORMAL;
	extern char *optarg;
	extern int optind;
	int ch;

	argc--, argv++;
	if (argc > 0 && eq(argv[0], "-a")) {
		fmt = ALL;
		argc--, argv++;
	}
	if (argc > 0 && eq(argv[0], "-g")) {
		fmt = GFMT;
		argc--, argv++;
	}
	if (argc > 0 && eq(argv[0], "-f")) {
		argc--, argv++;
		if ((ctl = open(argv[0], O_RDONLY | O_NONBLOCK)) < 0)
			syserrexit(*argv);
		argc--, argv++;
	}
	if (ioctl(ctl, TIOCGETD, &ldisc) < 0)
		syserrexit("TIOCGETD");
	if (tcgetattr(ctl, &t) < 0)
		syserrexit("tcgetattr");
	if (ioctl(ctl, TIOCGWINSZ, &win) < 0)
		warning("TIOCGWINSZ: %s", strerror(errno));
	checkredirect();	/* conversion aid */

	if (argc == 0 || fmt) {
		prmode(&t, ldisc, fmt);
		exit(0);
	}
	
	while (*argv) {
		if (eq("everything", *argv)) {
			prmode(&t, ldisc, ALL_BSD);
			exit(0);
		}
		if (eq("all", *argv)) {
			prmode(&t, ldisc, ALL);
			exit(0);
		}
		if (eq("tty", *argv) || eq("old", *argv) || eq("new", *argv)) {
			int nldisc = TTYDISC;

			if (ioctl(0, TIOCSETD, &nldisc) < 0)
				syserrexit("TIOCSETD");
			goto next;
		}
		if (eq("nl", *argv)) {
			t.c_iflag &= ~ICRNL;
			t.c_oflag &= ~ONLCR;
			goto next;
		}
		if (eq("-nl", *argv)) {
			t.c_iflag |= ICRNL;
			t.c_oflag |= ONLCR;
			goto next;
		}
		if (eq("dec", *argv)){
			t.c_cc[VERASE] = (u_char)0177;
			t.c_cc[VKILL] = CTRL('u');
			t.c_cc[VINTR] = CTRL('c');
			t.c_lflag &= ~ECHOPRT;
			t.c_lflag |= ECHOE|ECHOKE|ECHOCTL;
			t.c_iflag &= ~IXANY;
			goto next;
		}
		if (eq("raw", *argv)) {
			cfmakeraw(&t);
			t.c_cflag &= ~(CSIZE|PARENB);
			t.c_cflag |= CS8;
			goto next;
		}
		if (eq("cbreak", *argv)) {
			t.c_iflag |  BRKINT|IXON|IMAXBEL;
			t.c_oflag |= OPOST;
			t.c_lflag |= ISIG|IEXTEN;
			t.c_lflag &= ~ICANON;
		}
		if (eq("cooked", *argv) || eq("-raw", *argv) ||
		    eq("sane", *argv) || eq("-cbreak", *argv)) {
			t.c_cflag = TTYDEF_CFLAG | (t.c_cflag & CLOCAL);
			t.c_iflag = TTYDEF_IFLAG;
			t.c_iflag |= ICRNL;
			/* preserve user-preference flags in lflag */
#define	LKEEP	(ECHOKE|ECHOE|ECHOK|ECHOPRT|ECHOCTL|ALTWERASE|TOSTOP|NOFLSH)
			t.c_lflag = TTYDEF_LFLAG | (t.c_lflag & LKEEP);
			t.c_oflag = TTYDEF_OFLAG;
			goto next;
		}
		if (eq("rows", *argv)) {
			if (*(argv+1) == 0)
				goto setit;
			win.ws_row = atoi(*++argv);
			goto next;
		}
		if (eq("ispeed", *argv)) {
			int code;
			if (*(argv+1) == 0)
				errexit("missing ispeed");
			cfsetispeed(&t, atoi(*++argv));
			goto next;
		}
		if (eq("ospeed", *argv)) {
			if (*(argv+1) == 0)
				errexit("missing ospeed");
			cfsetospeed(&t, atoi(*++argv));
			goto next;
		}
		if (eq("cols", *argv) || eq("columns", *argv)) {
			if (*(argv+1) == 0)
				goto setit;
			win.ws_col = atoi(*++argv);
			goto next;
		}
		if (eq("size", *argv)) {
			put("%d %d\n", win.ws_row, win.ws_col);
			exit(0);
		}
		if (eq("extrpc", *argv) || eq("-extproc", *argv)) {
			if (**argv == '-')
				extproc = 0;
			else
				extproc = 1;
			ioctl(ctl, TIOCEXT, &extproc);
		}
		if (eq("speed", *argv)) {
			put("%d\n", cfgetospeed(&t));
			exit(0);
		}
		for (i=0; imodes[i].name; i++)
			if (eq(imodes[i].name, *argv)) {
				t.c_iflag &= ~imodes[i].unset;
				t.c_iflag |= imodes[i].set;
				goto next;
			}
		for (i=0; omodes[i].name; i++)
			if (eq(omodes[i].name, *argv)) {
				t.c_oflag &= ~omodes[i].unset;
				t.c_oflag |= omodes[i].set;
				goto next;
			}
		for (i=0; cmodes[i].name; i++)
			if (eq(cmodes[i].name, *argv)) {
				t.c_cflag &= ~cmodes[i].unset;
				t.c_cflag |= cmodes[i].set;
				goto next;
			}
		for (i=0; lmodes[i].name; i++)
			if (eq(lmodes[i].name, *argv)) {
				t.c_lflag &= ~lmodes[i].unset;
				t.c_lflag |= lmodes[i].set;
				goto next;
			}
		for (i=0; *cchars[i].names; i++) {
			char **cp = cchars[i].names;
			while (*cp) {
				if (eq(*cp, *argv)) {
					if (*++argv == 0)
						goto setit;
					if (eq(*argv, "undef") || 
					    eq(*argv, "disable"))
						t.c_cc[cchars[i].sub] = 
						   _POSIX_VDISABLE;
					else if (**argv == '^')
						t.c_cc[cchars[i].sub] = 
						    ((*argv)[1] == '?') ? 0177 :
						    ((*argv)[1] == '-') ?
						     _POSIX_VDISABLE :
						     (*argv)[1] & 037;
					else
						t.c_cc[cchars[i].sub] = **argv;
					goto next;
				}
				cp++;
			}
		}
		if (isdigit(**argv)) {
			cfsetospeed(&t, atoi(*argv));
			cfsetispeed(&t, atoi(*argv));
			goto next;
		}
		if (strncmp(*argv, "-gfmt", sizeof ("-gfmt") - 1) == 0) {
			gfmtset(&t, *argv);
			goto next;
		}
		/* didn't match anything */
		errexit("unknown option: %s", *argv);
		exit(1);
next:
		argv++;
	}
setit:
	if (tcsetattr(ctl, 0, &t) < 0)
		syserrexit("tcsetattr");
	if (ioctl(ctl, TIOCSWINSZ, &win) < 0)
		warning("can't set window size");

	exit(0);
}

gfmtset(tp, s) 
	register struct termios *tp;
	char *s;
{
	register int cnt;
	char sep;
	char *saves = s;
	int cval;
#define advance(c)	while (*(s) && *(s) != (c)) (s)++; if (*s) (s)++ ; \
				else \
					errexit("bad gfmt operand: %s", saves)
#define chkeq(string)	if (strncmp(s, (string), strlen(string))) \
				errexit("bad gfmt operand: %s", saves)

	if (s == NULL)
		errexit("missing gfmt string");
	advance(':');
	chkeq("iflag=");
	advance('=');
	sscanf(s, "%x", &tp->c_iflag);

	advance(':');
	chkeq("oflag");
	advance('=');
	sscanf(s, "%x", &tp->c_oflag);

	advance(':');
	chkeq("cflag");
	advance('=');
	sscanf(s, "%x", &tp->c_cflag);

	advance(':');
	chkeq("lflag");
	advance('=');
	sscanf(s, "%x", &tp->c_lflag);

	advance(':');
	chkeq("cc=");

	for (cnt = 0, sep = '='; cnt < NCCS; cnt++, sep = ',') {
		advance(sep);
		sscanf(s, "%o", &cval);
		tp->c_cc[cnt] = cval;
	}

	advance(':');
	chkeq("ispeed=");
	advance('=');
	sscanf(s, "%d", &tp->c_ispeed);

	advance(':');
	chkeq("ospeed=");
	advance('=');
	sscanf(s, "%d", &tp->c_ospeed);
}

prmode(tp, ldisc, fmt)
	struct termios *tp;
{
	long	i = tp->c_iflag,
		o = tp->c_oflag,
		c = tp->c_cflag,
		l = tp->c_lflag;
	u_char	*cc = tp->c_cc;
	int	ispeed = cfgetispeed(tp),
		ospeed = cfgetospeed(tp);
	char	unknown[32],
		*ld;
	char *ccval();

	if (fmt == GFMT) {
		int	cnt;
		char	sep;

		printf("-gfmt:iflag=%x:oflag=%x:cflag=%x:lflag=%x:cc",
			i, o, c, l);
		for (cnt = 0, sep = '='; cnt < NCCS; cnt++, sep = ',')
			printf("%c%o", sep, cc[cnt]);
		printf(":ispeed=%d:ospeed=%d:\n", ispeed, ospeed);
		return;
	}
	
	/*
	 * line discipline
	 */
	if (ldisc != TTYDISC) {
		switch(ldisc) {
		case TABLDISC:	
			ld = "tablet"; 
			break;
		case SLIPDISC:	
			ld = "slip"; 
			break;
		default:	
			sprintf(unknown, "#%d", ldisc);
			ld = unknown;
			break;
		}
		put("%s disc; ", ld);
	}
	/*
	 * line speed
	 */
	if (ispeed != ospeed)
		put("ispeed %d baud; ospeed %d baud;",
		     ispeed, ospeed);
	else
		put("speed %d baud;", ispeed);
	if (fmt) 
		put(" %d rows; %d columns;", win.ws_row, win.ws_col);
	put("\n");

#define lput(n, f, d) if (fmt || on(f) != d) mdput(n+on(f))
	/*
	 * "local" flags
	 */
#define on(f)	((l&f) != 0)
	if (debug) mdput("LFLAG: ");
	lput("-icanon ",ICANON, 1);
	lput("-isig ", ISIG, 1);
	lput("-iexten ", IEXTEN, 1);
	lput("-echo ",ECHO, 1);
	lput("-echoe ",ECHOE, 0);
	lput("-echok ",ECHOK, 0);
	lput("-echoke ",ECHOKE, 0);
	lput("-echonl ",ECHONL, 0);
	lput("-echoctl ",ECHOCTL, 0);
	lput("-echoprt ",ECHOPRT, 0);
	lput("-altwerase ",ALTWERASE, 0);
	lput("-noflsh ",NOFLSH, 0);
	lput("-tostop ",TOSTOP, 0);
	lput("-mdmbuf ",MDMBUF, 0);
	lput("-flusho ",FLUSHO, 0);
	lput("-pendin ",PENDIN, 0);
	lput("-nokerninfo ",NOKERNINFO, 0);
	lput("-extproc ",EXTPROC, 0);
	/*
	 * input flags
	 */
#undef on
#define on(f)	((i&f) != 0)
	mdput(0);
	if (debug) mdput("IFLAG: ");
	lput("-istrip ", ISTRIP, 0);
	lput("-icrnl ", ICRNL, 1);
	lput("-inlcr ", INLCR, 0);
	lput("-igncr ", IGNCR, 0);
	lput("-ixon ", IXON, 1);
	lput("-ixoff ", IXOFF, 0);
	lput("-ixany ", IXANY, 1);
	lput("-imaxbel ", IMAXBEL, 1);
	lput("-ignbrk ", IGNBRK, 0);
	lput("-brkint ", BRKINT, 1);
	lput("-inpck ", INPCK, 0);
	lput("-ignpar ", IGNPAR, 0);
	lput("-parmrk ", PARMRK, 0);
#undef on
	/*
	 * output flags
	 */
#define on(f)	((o&f) != 0)
	mdput(0);
	if (debug) mdput("OFLAG: ");
	lput("-opost ", OPOST, 1);
	lput("-onlcr ", ONLCR, 1);
	lput("-oxtabs ", OXTABS, 1);
#undef on
	/*
	 * control flags (hardware state)
	 */
#define on(f)	((c&f) != 0)
	mdput(0);
	if (debug) mdput("CFLAG: ");
	lput("-cread ", CREAD, 1);
	switch(c&CSIZE) {
	case CS5: mdput("cs5 "); break;
	case CS6: mdput("cs6 "); break;
	case CS7: mdput("cs7 "); break;
	case CS8: mdput("cs8 "); break;
	}
	mdput("-parenb "+on(PARENB));
	lput("-parodd ", PARODD, 0);
	lput("-hupcl ", HUPCL, 1);
	lput("-clocal ", CLOCAL, 0);
	lput("-cstopb ", CSTOPB, 0);
	lput("-crtscts ", CRTSCTS, 0);
	mdput(0);
#undef on
	/*
	 * special control characters
	 */
	if (debug) mdput("CCHARS: ");
	if (fmt != 2) {
		for (i=0; *cchars[i].names; i++) {
			char temp[64];

			if (fmt || cc[cchars[i].sub] != cchars[i].def) {
				sprintf(temp, "%s = %s; ", *cchars[i].names,
					ccval(cc[cchars[i].sub]), fmt);
				mdput(temp);
			}
		}
		mdput(0);
	} else {
		for (i=0; *cchars[i].names; i++)
			put("%*s", strlen(*(cchars[i].names+1)) + (i>0?1:0),
				*(cchars[i].names+1));
		printf("\n");
		for (i=0; *cchars[i].names; i++)
			put("%*s", strlen(*(cchars[i].names+1)) + (i>0?1:0),
				ccval(cc[cchars[i].sub], fmt));
		printf("\n");
	}
}

/*
 * gross, but since we're changing the control descriptor
 * from 1 to 0, most users will be probably be doing
 * "stty > /dev/sometty" by accident. If 1 and 2 are both ttys, 
 * but not the same, assume that 1 was incorrectly redirected.
 */
checkredirect() {
	struct stat st1, st2;

	if (isatty(1) && isatty(2) && fstat(1, &st1) != -1 && 
	    fstat(2, &st2) != -1 && (st1.st_rdev != st2.st_rdev))
warning("stdout appears redirected, but stdin is the control descriptor");
}

char *
ccval(c, fmt)
	unsigned char c;
{
	static char buf[128];
	char *bp;

	*buf = 0, bp = buf;
	if (c == _POSIX_VDISABLE)
		if (fmt == 2)
			return("<u>");
		else
			return("<undef>");
	if (c & 0200) {
		strcat(buf, "M-");
		*bp++ = 'M';
		*bp++ = '-';
		c &= 0177;
	}
	if (c == 0177) {
		*bp++ = '^';
		*bp++ = '?';
	}
	else if (c < 040) {
		*bp++ = '^';
		*bp++ = c + '@';
	}
	else
		*bp++ = c;
	*bp = 0;
	return(buf);
}


mdput(s)
	char *s;
{
	static int col = 0;

	if (s == (char *)0) {
		if (col) {
			put("\n");
			col = 0;
		}
		return;
	}
	if ((col += strlen(s)) > WRAPCOL) {
		put("\n");
		col = strlen(s);
	}
	put(s);
}

#include <varargs.h>

put(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;

	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vfprintf(OUT, fmt, ap);
	va_end(ap);
}


warning(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;

	fprintf(ERR, "stty: warning: ");
	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vfprintf(ERR, fmt, ap);
	va_end(ap);
	fprintf(ERR, "\n");
}


errexit(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;

	fprintf(ERR, "stty: ");
	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vfprintf(ERR, fmt, ap);
	va_end(ap);
	fprintf(ERR, "\n");
	exit(1);
}


syserrexit(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;

	fprintf(ERR, "stty: ");
	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vfprintf(ERR, fmt, ap);
	va_end(ap);
	fprintf(ERR, ": %s\n", strerror(errno));
	exit(1);
}
