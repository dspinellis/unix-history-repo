static char *sccsid ="@(#)stty.c	4.10 (Berkeley) %G%";
/*
 * set teletype modes
 */

#include <stdio.h>
#include <sgtty.h>

struct {
	char	*string;
	int	speed;
} speeds[] = {
	{ "0",		B0 },
	{ "50",		B50 },
	{ "75",		B75 },
	{ "110",	B110 },
	{ "134",	B134 },
	{ "134.5",	B134 },
	{ "150",	B150 },
	{ "200",	B200 },
	{ "300",	B300 },
	{ "600",	B600 },
	{ "1200",	B1200 },
	{ "1800",	B1800 },
	{ "2400",	B2400 },
	{ "4800",	B4800 },
	{ "9600",	B9600 },
	{ "exta",	EXTA },
	{ "19200",	EXTA },
	{ "extb",	EXTB },
	{ "38400",	EXTB },
	{ 0 },
};

struct {
	char	*string;
	long	set;
	long	reset;
} modes[] = {
	{ "even",	EVENP,		0 },
	{ "-even",	0,		EVENP },
	{ "odd",	ODDP,		0 },
	{ "-odd",	0,		ODDP },
	{ "raw",	RAW,		0 },
	{ "-raw",	0,		RAW },
	{ "cooked",	0,		RAW },
	{ "-nl",	CRMOD,		0 },
	{ "nl",		0,		CRMOD },
	{ "echo",	ECHO,		0 },
	{ "-echo",	0,		ECHO },
	{ "LCASE",	LCASE,		0 },
	{ "lcase",	LCASE,		0 },
	{ "-LCASE",	0,		LCASE },
	{ "-lcase",	0,		LCASE },
	{ "-tabs",	XTABS,		0 },
	{ "tabs",	0,		XTABS },
	{ "tandem",	TANDEM,		0 },
	{ "-tandem",	0,		TANDEM },
	{ "cbreak",	CBREAK,		0 },
	{ "-cbreak",	0,		CBREAK },
	{ "cr0",	CR0,		CR3 },
	{ "cr1",	CR1,		CR3 },
	{ "cr2",	CR2,		CR3 },
	{ "cr3",	CR3,		CR3 },
	{ "tab0",	TAB0,		XTABS },
	{ "tab1",	TAB1,		XTABS },
	{ "tab2",	TAB2,		XTABS },
	{ "nl0",	NL0,		NL3 },
	{ "nl1",	NL1,		NL3 },
	{ "nl2",	NL2,		NL3 },
	{ "nl3",	NL3,		NL3 },
	{ "ff0",	FF0,		FF1 },
	{ "ff1",	FF1,		FF1 },
	{ "bs0",	BS0,		BS1 },
	{ "bs1",	BS1,		BS1 },
	{ "33",		CR1,		ALLDELAY },
	{ "tty33",	CR1,		ALLDELAY },
	{ "37",		FF1+CR2+TAB1+NL1, ALLDELAY },
	{ "tty37",	FF1+CR2+TAB1+NL1, ALLDELAY },
	{ "05",		NL2,		ALLDELAY },
	{ "vt05",	NL2,		ALLDELAY },
	{ "tn",		CR1,		ALLDELAY },
	{ "tn300",	CR1,		ALLDELAY },
	{ "ti",		CR2,		ALLDELAY },
	{ "ti700",	CR2,		ALLDELAY },
	{ "tek",	FF1,		ALLDELAY },
	{ "crtbs",	CRTBS,		PRTERA },
	{ "-crtbs",	0,		CRTBS },
	{ "prterase",	PRTERA,		CRTBS+CRTKIL+CRTERA },
	{ "-prterase",	0,		PRTERA },
	{ "crterase",	CRTERA,		PRTERA },
	{ "-crterase",	0,		CRTERA },
	{ "crtkill",	CRTKIL,		PRTERA },
	{ "-crtkill",	0,		CRTKIL },
	{ "tilde",	TILDE,		0 },
	{ "-tilde",	0,		TILDE },
	{ "mdmbuf",	MDMBUF,		0 },
	{ "-mdmbuf",	0,		MDMBUF },
	{ "litout",	LITOUT,		0 },
	{ "-litout",	0,		LITOUT },
	{ "tostop",	TOSTOP,		0 },
	{ "-tostop",	0,		TOSTOP },
	{ "flusho",	FLUSHO,		0 },
	{ "-flusho",	0,		FLUSHO },
	{ "nohang",	NOHANG,		0 },
	{ "-nohang",	0,		NOHANG },
#ifdef notdef
	{ "etxack",	ETXACK,		0 },
	{ "-etxack",	0,		ETXACK },
#endif
	{ "ctlecho",	CTLECH,		0 },
	{ "-ctlecho",	0,		CTLECH },
	{ "pendin",	PENDIN,		0 },
	{ "-pendin",	0,		PENDIN },
	{ "decctlq",	DECCTQ,		0 },
	{ "-decctlq",	0,		DECCTQ },
	{ "noflsh",	NOFLSH,		0 },
	{ "-noflsh",	0,		NOFLSH },
	{ 0 },
};

struct ttychars tc;
struct sgttyb sb;
long	flags;
int	oldisc, ldisc;

struct	special {
	char	*name;
	char	*cp;
	char	def;
} special[] = {
	{ "erase",	&tc.tc_erase,		CERASE },
	{ "kill",	&tc.tc_kill,		CKILL },
	{ "intr",	&tc.tc_intrc,		CINTR },
	{ "quit",	&tc.tc_quitc,		CQUIT },
	{ "start",	&tc.tc_startc,		CSTART },
	{ "stop",	&tc.tc_stopc,		CSTOP },
	{ "eof",	&tc.tc_eofc,		CEOF },
	{ "brk",	&tc.tc_brkc,		CBRK },
	{ "susp",	&tc.tc_suspc,		CSUSP },
	{ "dsusp",	&tc.tc_dsuspc,		CDSUSP },
	{ "rprnt",	&tc.tc_rprntc,		CRPRNT },
	{ "flush",	&tc.tc_flushc,		CFLUSH },
	{ "werase",	&tc.tc_werasc,		CWERASE },
	{ "lnext",	&tc.tc_lnextc,		CLNEXT },
	0
};
char	*arg;

int	argc;
char	**argv;

main(iargc, iargv)
	int iargc;
	char *iargv[];
{
	int i;
	register struct special *sp;
	char obuf[BUFSIZ];

	setbuf(stderr, obuf);
	argc = iargc;
	argv = iargv;
	ioctl(1, TIOCCGET, (char *)&tc);
	ioctl(1, TIOCGET, (char *)&flags);
	ioctl(1, TIOCGETD, &ldisc);
#ifndef notdef
	ioctl(1, TIOCGETP, (char *)&sb);
#endif
	oldisc = ldisc;
	if (argc == 1) {
		prmodes(0);
		exit(0);
	}
	if (argc == 2 && !strcmp(argv[1], "all")) {
		prmodes(1);
		exit(0);
	}
	if (argc == 2 && !strcmp(argv[1], "everything")) {
		prmodes(2);
		exit(0);
	}
	while (--argc > 0) {
		arg = *++argv;
		if (eq("ek")) {
			tc.tc_erase = '#';
			tc.tc_kill = '@';
			continue;
		}
		if (eq("new")) {
			ldisc = NTTYDISC;
			if (ioctl(1, TIOCSETD, &ldisc) < 0)
				perror("ioctl");
			continue;
		}
		if (eq("newcrt")) {
			ldisc = NTTYDISC;
			flags &= ~PRTERA;
			flags |= CRTBS|CTLECH;
			if (sb.sg_ospeed >= B1200)
				flags |= CRTERA|CRTKIL;
			if (ioctl(1, TIOCSETD, &ldisc) < 0)
				perror("ioctl");
			continue;
		}
		if (eq("crt")) {
			flags &= ~PRTERA;
			flags |= CRTBS|CTLECH;
			if (sb.sg_ospeed >= B1200)
				flags |= CRTERA|CRTKIL;
			continue;
		}
		if (eq("old")) {
			ldisc = OTTYDISC;
			if (ioctl(1, TIOCSETD, &ldisc) < 0)
				perror("ioctl");
			continue;
		}
		if (eq("dec")) {
			tc.tc_erase = 0177;
			tc.tc_kill = CTRL(u);
			tc.tc_intrc = CTRL(c);
			ldisc = NTTYDISC;
			flags &= ~PRTERA;
			flags |= CRTBS|CTLECH|DECCTQ;
			if (sb.sg_ospeed >= B1200)
				flags |= CRTERA|CRTKIL;
			if (ioctl(1, TIOCSETD, &ldisc) < 0)
				perror("ioctl");
			continue;
		}
		for (sp = special; sp->name; sp++)
			if (eq(sp->name)) {
				if (--argc == 0)
					goto done;
				if (**++argv == 'u')
					*sp->cp = 0377;
				else if (**argv == '^')
					*sp->cp = ((*argv)[1] == '?') ?
					    0177 : (*argv)[1] & 037;
				else
					*sp->cp = **argv;
				goto cont;
			}
		if (eq("gspeed")) {
			sb.sg_ispeed = B300;
			sb.sg_ospeed = B9600;
			continue;
		}
		if (eq("hup")) {
			if (ioctl(1, TIOCHPCL, NULL) < 0)
				perror("ioctl");
			continue;
		}
		for (i = 0; speeds[i].string; i++)
			if (eq(speeds[i].string)) {
				sb.sg_ispeed = sb.sg_ospeed = speeds[i].speed;
				goto cont;
			}
		if (eq("speed")) {
			int fd = open("/dev/tty", 0);

			if (fd < 0) {
				perror("open");
				exit(1);
			}
			ioctl(fd, TIOCGETP, &sb);
			for (i = 0; speeds[i].string; i++)
				if (sb.sg_ospeed == speeds[i].speed) {
					printf("%s\n", speeds[i].string);
					exit(0);
				}
			printf("unknown\n");
			exit(1);
		}
		for (i = 0; modes[i].string; i++)
			if (eq(modes[i].string)) {
				flags &= ~modes[i].reset;
				flags |= modes[i].set;
			}
		if (arg)
			fprintf(stderr,"unknown mode: %s\n", arg);
cont:
		;
	}
done:
#ifndef notdef
	ioctl(1, TIOCSETN, &sb);
#endif
	ioctl(1, TIOCSET, &flags);
	ioctl(1, TIOCCSET, &tc);
}

eq(string)
	char *string;
{
	int i;

	if (!arg)
		return (0);
	i = 0;
loop:
	if (arg[i] != string[i])
		return(0);
	if (arg[i++] != '\0')
		goto loop;
	arg = 0;
	return (1);
}

prmodes(all)
	int all;
{
	register m;
	int any;

	if (ldisc == NETLDISC)
		fprintf(stderr, "net discipline, ");
	else if (ldisc == NTTYDISC)
		fprintf(stderr, "new tty, ");
	else if (all == 2)
		fprintf(stderr, "old tty, ");
	if(sb.sg_ispeed != sb.sg_ospeed) {
		prspeed("input speed ", sb.sg_ispeed);
		prspeed("output speed ", sb.sg_ospeed);
	} else
		prspeed("speed ", sb.sg_ispeed);
	fprintf(stderr, all == 2 ? "\n" : "; ");
	m = flags;
	if (all == 2 || (m&(EVENP|ODDP)) != (EVENP|ODDP)) {
		if (m & EVENP)
			fprintf(stderr,"even ");
		if (m & ODDP)
			fprintf(stderr,"odd ");
	}
	if (all == 2 || m&RAW)
		fprintf(stderr,"-raw " + ((m&RAW) != 0));
	if (all == 2 || (m&CRMOD) == 0)
		fprintf(stderr,"-nl " + ((m&CRMOD) == 0));
	if (all == 2 || (m&ECHO) == 0)
		fprintf(stderr,"-echo " + ((m&ECHO) != 0));
	if (all == 2 || m&LCASE)
		fprintf(stderr,"-lcase " + ((m&LCASE) != 0));
	if (all == 2 || m&TANDEM)
		fprintf(stderr,"-tandem " + ((m&TANDEM) != 0));
	fprintf(stderr,"-tabs " + ((m&XTABS) != XTABS));
	if (all == 2 || m&CBREAK)
		fprintf(stderr,"-cbreak " + ((m&CBREAK) != 0));
	if (all == 2 || m&NLDELAY)
		delay((m&NLDELAY) / NL1, "nl");
	if ((m&TBDELAY) != XTABS)
		delay((m&TBDELAY)/ TAB1, "tab");
	if (all == 2 || m&CRDELAY)
		delay((m&CRDELAY) / CR1, "cr");
	if (all == 2 || m&VTDELAY)
		delay((m&VTDELAY) / FF1, "ff");
	if (all == 2 || m&BSDELAY)
		delay((m&BSDELAY) / BS1, "bs");
	if (all)
		fprintf(stderr,"\n");
#define	lpit(what,str) \
	if (all == 2 || flags&what) { \
		fprintf(stderr,str + ((flags&what) != 0)); any++; \
	}
	if (ldisc == NTTYDISC) {
		int newcrt = (flags&(CTLECH|CRTBS)) == (CTLECH|CRTBS) &&
		    (flags&(CRTERA|CRTKIL)) ==
		      ((sb.sg_ospeed > B300) ? CRTERA|CRTKIL : 0);
		if (newcrt) {
			fprintf(stderr, all != 2 ? "crt" :
				 "crt: (crtbs crterase crtkill ctlecho) ");
			any++;
		} else {
			lpit(CRTBS, "-crtbs ");
			lpit(CRTERA, "-crterase ");
			lpit(CRTKIL, "-crtkill ");
			lpit(CTLECH, "-ctlecho ");
			lpit(PRTERA, "-prterase ");
		}
		lpit(TOSTOP, "-tostop ");
		if (all == 2) {
			fprintf(stderr, "\n");
			any = 0;
		}
		lpit(TILDE, "-tilde ");
		lpit(FLUSHO, "-flusho ");
		lpit(MDMBUF, "-mdmbuf ");
		lpit(LITOUT, "-litout ");
		lpit(NOHANG, "-nohang ");
		if (any) {
			fprintf(stderr,"\n");
			any = 0;
		}
#ifdef notdef
		lpit(ETXACK, "-etxack ");
#endif
		lpit(PENDIN, "-pendin ");
		lpit(DECCTQ, "-decctlq ");
		lpit(NOFLSH, "-noflsh ");
		if (any)
			fprintf(stderr,"\n");
	} else if (!all)
		fprintf(stderr,"\n");
	if (all) {
		switch (ldisc) {

		case 0:
			fprintf(stderr,"\
erase  kill   intr   quit   stop   eof\
\n");
			pcol(tc.tc_erase, -1);
			pcol(tc.tc_kill, -1);
			pcol(tc.tc_intrc, -1);
			pcol(tc.tc_quitc, -1);
			pcol(tc.tc_stopc, tc.tc_startc);
			pcol(tc.tc_eofc, tc.tc_brkc);
			fprintf(stderr,"\n");
			break;

		case NTTYDISC:
			fprintf(stderr,"\
erase  kill   werase rprnt  flush  lnext  susp   intr   quit   stop   eof\
\n"); 
			pcol(tc.tc_erase, -1);
			pcol(tc.tc_kill, -1);
			pcol(tc.tc_werasc, -1);
			pcol(tc.tc_rprntc, -1);
			pcol(tc.tc_flushc, -1);
			pcol(tc.tc_lnextc, -1);
			pcol(tc.tc_suspc, tc.tc_dsuspc);
			pcol(tc.tc_intrc, -1);
			pcol(tc.tc_quitc, -1);
			pcol(tc.tc_stopc, tc.tc_startc);
			pcol(tc.tc_eofc, tc.tc_brkc);
			fprintf(stderr,"\n");
			break;
		}
	} else if (ldisc != NETLDISC) {
		register struct special *sp;
		int first = 1;

		for (sp = special; sp->name; sp++) {
			if ((*sp->cp&0377) != (sp->def&0377)) {
				pit(*sp->cp, sp->name, first ? "" : ", ");
				first = 0;
			};
			if (sp->cp == &tc.tc_brkc && ldisc == 0)
				break;
		}
		if (first == 0)
			fprintf(stderr, "\n");
	}
}

pcol(ch1, ch2)
	int ch1, ch2;
{
	int nout = 0;

	ch1 &= 0377;
	ch2 &= 0377;
	if (ch1 == ch2)
		ch2 = 0377;
	for (; ch1 != 0377 || ch2 != 0377; ch1 = ch2, ch2 = 0377) {
		if (ch1 == 0377)
			continue;
		if (ch1 & 0200) {
			fprintf(stderr, "M-");
			nout += 2;
			ch1 &= ~ 0200;
		}
		if (ch1 == 0177) {
			fprintf(stderr, "^");
			nout++;
			ch1 = '?';
		} else if (ch1 < ' ') {
			fprintf(stderr, "^");
			nout++;
			ch1 += '@';
		}
		fprintf(stderr, "%c", ch1);
		nout++;
		if (ch2 != 0377) {
			fprintf(stderr, "/");
			nout++;
		}
	}
	while (nout < 7) {
		fprintf(stderr, " ");
		nout++;
	}
}

pit(what, itsname, sep)
	unsigned what;
	char *itsname, *sep;
{

	what &= 0377;
	fprintf(stderr, "%s%s", sep, itsname);
	if (what == 0377) {
		fprintf(stderr, " <undef>");
		return;
	}
	fprintf(stderr, " = ");
	if (what & 0200) {
		fprintf(stderr, "M-");
		what &= ~ 0200;
	}
	if (what == 0177) {
		fprintf(stderr, "^");
		what = '?';
	} else if (what < ' ') {
		fprintf(stderr, "^");
		what += '@';
	}
	fprintf(stderr, "%c", what);
}

delay(m, s)
	char *s;
{

	if (m)
		fprintf(stderr,"%s%d ", s, m);
}

int	speed[] = {
	0,50,75,110,134,150,200,300,600,1200,1800,2400,4800,9600,19200,38400
};

prspeed(c, s)
char *c;
{

	fprintf(stderr,"%s%d baud",  c, speed[s]);
}
