/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)stty.c	5.6 (Berkeley) %G%";
#endif not lint

/*
 * set teletype modes
 */

#include <sys/ioctl.h>
#include <stdio.h>
#include "pathnames.h"

struct
{
	char	*string;
	int	speed;
} speeds[] = {
	"0",	B0,
	"50",	B50,
	"75",	B75,
	"110",	B110,
	"134",	B134,
	"134.5",B134,
	"150",	B150,
	"200",	B200,
	"300",	B300,
	"600",	B600,
	"1200",	B1200,
	"1800",	B1800,
	"2400",	B2400,
	"4800",	B4800,
	"9600",	B9600,
	"exta",	EXTA,
	"19200", EXTA,
	"extb",	EXTB,
	"38400", EXTB,
	0,
};
struct
{
	char	*string;
	int	set;
	int	reset;
	int	lset;
	int	lreset;
} modes[] = {
	"even",		EVENP, 0, 0, 0,
	"-even",	0, EVENP, 0, 0,
	"odd",		ODDP, 0, 0, 0,
	"-odd",		0, ODDP, 0, 0,
	"raw",		RAW, 0, 0, 0,
	"-raw",		0, RAW, 0, 0,
	"cooked",	0, RAW, 0, 0,
	"-nl",		CRMOD, 0, 0, 0,
	"nl",		0, CRMOD, 0, 0,
	"echo",		ECHO, 0, 0, 0,
	"-echo",	0, ECHO, 0, 0,
	"LCASE",	LCASE, 0, 0, 0,
	"lcase",	LCASE, 0, 0, 0,
	"-LCASE",	0, LCASE, 0, 0,
	"-lcase",	0, LCASE, 0, 0,
	"-tabs",	XTABS, 0, 0, 0,
	"tabs",		0, XTABS, 0, 0,
	"tandem",	TANDEM, 0, 0, 0,
	"-tandem",	0, TANDEM, 0, 0,
	"cbreak",	CBREAK, 0, 0, 0,
	"-cbreak",	0, CBREAK, 0, 0,
	"cr0",		CR0, CR3, 0, 0,
	"cr1",		CR1, CR3, 0, 0,
	"cr2",		CR2, CR3, 0, 0,
	"cr3",		CR3, CR3, 0, 0,
	"tab0",		TAB0, XTABS, 0, 0,
	"tab1",		TAB1, XTABS, 0, 0,
	"tab2",		TAB2, XTABS, 0, 0,
	"nl0",		NL0, NL3, 0, 0,
	"nl1",		NL1, NL3, 0, 0,
	"nl2",		NL2, NL3, 0, 0,
	"nl3",		NL3, NL3, 0, 0,
	"ff0",		FF0, FF1, 0, 0,
	"ff1",		FF1, FF1, 0, 0,
	"bs0",		BS0, BS1, 0, 0,
	"bs1",		BS1, BS1, 0, 0,
	"33",		CR1, ALLDELAY, 0, 0,
	"tty33",	CR1, ALLDELAY, 0, 0,
	"37",		FF1+CR2+TAB1+NL1, ALLDELAY, 0, 0,
	"tty37",	FF1+CR2+TAB1+NL1, ALLDELAY, 0, 0,
	"05",		NL2, ALLDELAY, 0, 0,
	"vt05",		NL2, ALLDELAY, 0, 0,
	"tn",		CR1, ALLDELAY, 0, 0,
	"tn300",	CR1, ALLDELAY, 0, 0,
	"ti",		CR2, ALLDELAY, 0, 0,
	"ti700",	CR2, ALLDELAY, 0, 0,
	"tek",		FF1, ALLDELAY, 0, 0,
	"crtbs",	0, 0, LCRTBS, LPRTERA,
	"-crtbs",	0, 0, 0, LCRTBS,
	"prterase",	0, 0, LPRTERA, LCRTBS+LCRTKIL+LCRTERA,
	"-prterase",	0, 0, 0, LPRTERA,
	"crterase",	0, 0, LCRTERA, LPRTERA,
	"-crterase",	0, 0, 0, LCRTERA,
	"crtkill",	0, 0, LCRTKIL, LPRTERA,
	"-crtkill",	0, 0, 0, LCRTKIL,
	"tilde",	0, 0, LTILDE, 0,
	"-tilde",	0, 0, 0, LTILDE,
	"mdmbuf",	0, 0, LMDMBUF, 0,
	"-mdmbuf",	0, 0, 0, LMDMBUF,
	"litout",	0, 0, LLITOUT, 0,
	"-litout",	0, 0, 0, LLITOUT,
	"pass8",	0, 0, LPASS8, 0,
	"-pass8",	0, 0, 0, LPASS8,
	"tostop",	0, 0, LTOSTOP, 0,
	"-tostop",	0, 0, 0, LTOSTOP,
	"flusho",	0, 0, LFLUSHO, 0,
	"-flusho",	0, 0, 0, LFLUSHO,
	"nohang",	0, 0, LNOHANG, 0,
	"-nohang",	0, 0, 0, LNOHANG,
#ifdef notdef
	"etxack",	0, 0, LETXACK, 0,
	"-etxack",	0, 0, 0, LETXACK,
#endif
	"ctlecho",	0, 0, LCTLECH, 0,
	"-ctlecho",	0, 0, 0, LCTLECH,
	"pendin",	0, 0, LPENDIN, 0,
	"-pendin",	0, 0, 0, LPENDIN,
	"decctlq",	0, 0, LDECCTQ, 0,
	"-decctlq",	0, 0, 0, LDECCTQ,
	"noflsh",	0, 0, LNOFLSH, 0,
	"-noflsh",	0, 0, 0, LNOFLSH,
	0,
};

struct tchars tc;
struct ltchars ltc;
struct sgttyb mode;
struct winsize win;
int	lmode;
int	oldisc, ldisc;

struct	special {
	char	*name;
	char	*cp;
	char	def;
} special[] = {
	"erase",	&mode.sg_erase,		CERASE,
	"kill",		&mode.sg_kill,		CKILL,
	"intr",		&tc.t_intrc,		CINTR,
	"quit",		&tc.t_quitc,		CQUIT,
	"start",	&tc.t_startc,		CSTART,
	"stop",		&tc.t_stopc,		CSTOP,
	"eof",		&tc.t_eofc,		CEOF,
	"brk",		&tc.t_brkc,		CBRK,
	"susp",		&ltc.t_suspc,		CSUSP,
	"dsusp",	&ltc.t_dsuspc,		CDSUSP,
	"rprnt",	&ltc.t_rprntc,		CRPRNT,
	"flush",	&ltc.t_flushc,		CFLUSH,
	"werase",	&ltc.t_werasc,		CWERASE,
	"lnext",	&ltc.t_lnextc,		CLNEXT,
	0
};
char	*arg;

int	argc;
char	**argv;
main(iargc, iargv)
char	**iargv;
{
	int i;
	register struct special *sp;
	char obuf[BUFSIZ];

	setbuf(stderr, obuf);
	argc = iargc;
	argv = iargv;
	ioctl(1, TIOCGETP, &mode);
	ioctl(1, TIOCGETD, &ldisc);
	oldisc = ldisc;
	ioctl(1, TIOCGETC, &tc);
	ioctl(1, TIOCLGET, &lmode);
	ioctl(1, TIOCGLTC, &ltc);
	ioctl(1, TIOCGWINSZ, &win);
	if(argc == 1) {
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
/*
	if (argc == 2 && !strcmp(argv[1], "all")) {
		prmodes(2);
		exit(0);
	}
*/
	while(--argc > 0) {
		arg = *++argv;
		if (eq("ek")){
			mode.sg_erase = '#';
			mode.sg_kill = '@';
			continue;
		}
		if (eq("new")){
			ldisc = NTTYDISC;
			if (ioctl(1, TIOCSETD, &ldisc)<0)
				perror("ioctl");
			continue;
		}
		if (eq("newcrt")){
			ldisc = NTTYDISC;
			lmode &= ~LPRTERA;
			lmode |= LCRTBS|LCTLECH;
			if (mode.sg_ospeed >= B1200)
				lmode |= LCRTERA|LCRTKIL;
			if (ioctl(1, TIOCSETD, &ldisc)<0)
				perror("ioctl");
			continue;
		}
		if (eq("crt")){
			lmode &= ~LPRTERA;
			lmode |= LCRTBS|LCTLECH;
			if (mode.sg_ospeed >= B1200)
				lmode |= LCRTERA|LCRTKIL;
			continue;
		}
		if (eq("old")){
			ldisc = 0;
			if (ioctl(1, TIOCSETD, &ldisc)<0)
				perror("ioctl");
			continue;
		}
		if (eq("dec")){
			mode.sg_erase = 0177;
			mode.sg_kill = CKILL;
			tc.t_intrc = CINTR;
			ldisc = NTTYDISC;
			lmode &= ~LPRTERA;
			lmode |= LCRTBS|LCTLECH|LDECCTQ;
			if (mode.sg_ospeed >= B1200)
				lmode |= LCRTERA|LCRTKIL;
			if (ioctl(1, TIOCSETD, &ldisc)<0)
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
			mode.sg_ispeed = B300;
			mode.sg_ospeed = B9600;
			continue;
		}
		if (eq("hup")) {
			ioctl(1, TIOCHPCL, NULL);
			continue;
		}
		if (eq("rows")) {
			if (--argc == 0)
				goto done;
			win.ws_row = atoi(*++argv);
		}
		if (eq("cols") || eq("columns")) {
			if (--argc == 0)
				goto done;
			win.ws_col = atoi(*++argv);
		}
		if (eq("size")) {
			ioctl(open(_PATH_DEVTTY, 0), TIOCGWINSZ, &win);
			printf("%d %d\n", win.ws_row, win.ws_col);
			exit(0);
		}
		for(i=0; speeds[i].string; i++)
			if(eq(speeds[i].string)) {
				mode.sg_ispeed = mode.sg_ospeed = speeds[i].speed;
				goto cont;
			}
		if (eq("speed")) {
			ioctl(open(_PATH_DEVTTY, 0), TIOCGETP, &mode);
			for(i=0; speeds[i].string; i++)
				if (mode.sg_ospeed == speeds[i].speed) {
					printf("%s\n", speeds[i].string);
					exit(0);
				}
			printf("unknown\n");
			exit(1);
		}
		for(i=0; modes[i].string; i++)
			if(eq(modes[i].string)) {
				mode.sg_flags &= ~modes[i].reset;
				mode.sg_flags |= modes[i].set;
				lmode &= ~modes[i].lreset;
				lmode |= modes[i].lset;
			}
		if(arg)
			fprintf(stderr,"unknown mode: %s\n", arg);
cont:
		;
	}
done:
	ioctl(1, TIOCSETN, &mode);
	ioctl(1, TIOCSETC, &tc);
	ioctl(1, TIOCSLTC, &ltc);
	ioctl(1, TIOCLSET, &lmode);
	ioctl(1, TIOCSWINSZ, &win);
}

eq(string)
char *string;
{
	int i;

	if(!arg)
		return(0);
	i = 0;
loop:
	if(arg[i] != string[i])
		return(0);
	if(arg[i++] != '\0')
		goto loop;
	arg = 0;
	return(1);
}

prmodes(all)
{
	register m;
	int any;

	if(ldisc==NETLDISC)
		fprintf(stderr, "net discipline, ");
	else if(ldisc==NTTYDISC)
		fprintf(stderr, "new tty, ");
	else if(all==2)
		fprintf(stderr, "old tty, ");
	if(mode.sg_ispeed != mode.sg_ospeed) {
		prspeed("input speed ", mode.sg_ispeed);
		prspeed("output speed ", mode.sg_ospeed);
	} else
		prspeed("speed ", mode.sg_ispeed);
	if (all)
		fprintf(stderr, ", %d rows, %d columns", win.ws_row, win.ws_col);
	fprintf(stderr, all==2 ? "\n" : "; ");
	m = mode.sg_flags;
	if(all==2 || (m&(EVENP|ODDP))!=(EVENP|ODDP)) {
		if(m & EVENP)	fprintf(stderr,"even ");
		if(m & ODDP)	fprintf(stderr,"odd ");
	}
	if(all==2 || m&RAW)
		fprintf(stderr,"-raw "+((m&RAW)!=0));
	if(all==2 || (m&CRMOD)==0)
		fprintf(stderr,"-nl "+((m&CRMOD)==0));
	if(all==2 || (m&ECHO)==0)
		fprintf(stderr,"-echo "+((m&ECHO)!=0));
	if(all==2 || (m&LCASE))
		fprintf(stderr,"-lcase "+((m&LCASE)!=0));
	if(all==2 || (m&TANDEM))
		fprintf(stderr,"-tandem "+((m&TANDEM)!=0));
	fprintf(stderr,"-tabs "+((m&XTABS)!=XTABS));
	if(all==2 || (m&CBREAK))
		fprintf(stderr,"-cbreak "+((m&CBREAK)!=0));
	if(all==2 || (m&NLDELAY))
		delay((m&NLDELAY)/NL1,	"nl");
	if ((m&TBDELAY)!=XTABS)
		delay((m&TBDELAY)/TAB1,	"tab");
	if(all==2 || (m&CRDELAY))
		delay((m&CRDELAY)/CR1,	"cr");
	if(all==2 || (m&VTDELAY))
		delay((m&VTDELAY)/FF1,	"ff");
	if(all==2 || (m&BSDELAY))
		delay((m&BSDELAY)/BS1,	"bs");
	if (all)
		fprintf(stderr,"\n");
#define	lpit(what,str) \
	if (all==2||(lmode&what)) { \
		fprintf(stderr,str+((lmode&what)!=0)); any++; \
	}
	if (ldisc == NTTYDISC) {
		int newcrt = (lmode&(LCTLECH|LCRTBS)) == (LCTLECH|LCRTBS) &&
		    (lmode&(LCRTERA|LCRTKIL)) ==
		      ((mode.sg_ospeed > B300) ? LCRTERA|LCRTKIL : 0);
		int nothing = 1;
		if (newcrt) {
			if (all==2)
				fprintf(stderr, "crt: (crtbs crterase crtkill ctlecho) ");
			else
				fprintf(stderr, "crt ");
			any++;
		} else {
			lpit(LCRTBS, "-crtbs ");
			lpit(LCRTERA, "-crterase ");
			lpit(LCRTKIL, "-crtkill ");
			lpit(LCTLECH, "-ctlecho ");
			lpit(LPRTERA, "-prterase ");
		}
		lpit(LTOSTOP, "-tostop ");
		if (all==2) {
			fprintf(stderr, "\n");
			any = 0;
			nothing = 0;
		}
		lpit(LTILDE, "-tilde ");
		lpit(LFLUSHO, "-flusho ");
		lpit(LMDMBUF, "-mdmbuf ");
		lpit(LLITOUT, "-litout ");
		lpit(LPASS8, "-pass8 ");
		lpit(LNOHANG, "-nohang ");
		if (any) {
			fprintf(stderr,"\n");
			any = 0;
			nothing = 0;
		}
#ifdef notdef
		lpit(LETXACK, "-etxack ");
#endif
		lpit(LPENDIN, "-pendin ");
		lpit(LDECCTQ, "-decctlq ");
		lpit(LNOFLSH, "-noflsh ");
		if (any || nothing)
			fprintf(stderr,"\n");
	} else if (!all)
		fprintf(stderr,"\n");
	if (all) {
		switch (ldisc) {

		case 0:
			fprintf(stderr,"\
erase  kill   intr   quit   stop   eof\
\n");
			pcol(mode.sg_erase, -1);
			pcol(mode.sg_kill, -1);
			pcol(tc.t_intrc, -1);
			pcol(tc.t_quitc, -1);
			pcol(tc.t_stopc, tc.t_startc);
			pcol(tc.t_eofc, tc.t_brkc);
			fprintf(stderr,"\n");
			break;

		case NTTYDISC:
			fprintf(stderr,"\
erase  kill   werase rprnt  flush  lnext  susp   intr   quit   stop   eof\
\n"); 
			pcol(mode.sg_erase, -1);
			pcol(mode.sg_kill, -1);
			pcol(ltc.t_werasc, -1);
			pcol(ltc.t_rprntc, -1);
			pcol(ltc.t_flushc, -1);
			pcol(ltc.t_lnextc, -1);
			pcol(ltc.t_suspc, ltc.t_dsuspc);
			pcol(tc.t_intrc, -1);
			pcol(tc.t_quitc, -1);
			pcol(tc.t_stopc, tc.t_startc);
			pcol(tc.t_eofc, tc.t_brkc);
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
			if (sp->cp == &tc.t_brkc && ldisc == 0)
				break;
		}
		if (!first)
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

	if(m)
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
