#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include "tdef.h"
extern
#include "d.h"
extern
#include "v.h"
#ifdef NROFF
extern
#include "tw.h"
#endif
#include "s.h"
#include <setjmp.h>
jmp_buf sjbuf;
#include	<sgtty.h>
/*
troff1.c

consume options, initialization, main loop,
input routines, escape function calling
*/

#include "ext.h"

char	*sprintf();
char	*getenv();
tchar	inchar[LNSIZE], *pinchar = inchar;	/* XXX */
filep ipl[NSO];
long	offl[NSO];
long	ioff;
char	*ttyp;
extern struct contab {
	int	rq;
	union {
		int	(*f)();
		unsigned	mx;
	} x;
} contab[NM];

main(argc, argv)
int	argc;
char	**argv;
{
	register char	*p, *q;
	register j;
	tchar i;
	extern catch(), kcatch();
	int	oargc;
	char	**oargv;

	signal(SIGHUP, catch);
	if (signal(SIGINT, catch) == SIG_IGN) {
		signal(SIGHUP, SIG_IGN);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
	}
	signal(SIGPIPE, catch);
	signal(SIGTERM, kcatch);
	oargc = argc;
	oargv = argv;
	if (p = getenv("PRINTER")) strcpy(devname, p);
	if (p = getenv("TYPESETTER")) strcpy(devname, p);
	for (nfi = 0, p = nextf; *p; p++) nfi++;
	init0();
options:
	while (--argc > 0 && (++argv)[0][0] == '-')
		switch (argv[0][1]) {

		case 'F':	/* switch font tables from default */
			if (argv[0][2] != '\0') {
				strcpy(termtab, &argv[0][2]);
				strcpy(fontfile, &argv[0][2]);
			} else {
				argv++; argc--;
				strcpy(termtab, argv[0]);
				strcpy(fontfile, argv[0]);
			}
			continue;
		case 0:
			goto start;
		case 'i':
			stdi++;
			continue;
		case 'q':
			quiet++;
			if (gtty(0, &ttys) >= 0)
				ttysave = ttys.sg_flags;
			continue;
		case 'n':
			npn = ctoi(&argv[0][2]);
			continue;
		case 'u':	/* set emboldening amount */
			bdtab[3] = ctoi(&argv[0][2]);
			if (bdtab[3] < 0 || bdtab[3] > 50)
				bdtab[3] = 0;
			continue;
		case 's':
			if (!(stop = ctoi(&argv[0][2])))
				stop++;
			continue;
		case 'r':
			eibuf = sprintf(ibuf+strlen(ibuf), ".nr %c %s\n",
				argv[0][2], &argv[0][3]);
			continue;
		case 'm':
			p = &nextf[nfi];
			q = &argv[0][2];
			while ((*p++ = *q++) != 0)
				;
			mflg++;
			continue;
		case 'o':
			getpn(&argv[0][2]);
			continue;
#ifdef NROFF
		case 'h':
			hflg++;
			continue;
		case 'z':
			no_out++;
			continue;
		case 'e':
			eqflg++;
			continue;
		case 'T':
			strcat(termtab, &argv[0][2]);
			dotT++;
			continue;
#endif
#ifndef NROFF
		case 'P':
		case 'T':
			strcpy(devname, &argv[0][2]);
			dotT++;
			continue;
		case 'z':
			no_out++;
		case 'a':
			ascii = 1;
			nofeed++;
		case 't':
			ptid = 1;
			continue;
		case 'f':
			nofeed++;
			continue;
#endif
		default:
			fprintf(stderr, "troff: unknown option %s\n", argv[0]);
			done(02);
		}

start:
	init1(oargv[0][0]);
	argp = argv;
	rargc = argc;
	init2();
	setjmp(sjbuf);
loop:
	copyf = lgf = nb = nflush = nlflg = 0;
	if (ip && rbf0(ip) == 0 && ejf && frame->pframe <= ejl) {
		nflush++;
		trap = 0;
		eject((struct s *)0);
		goto loop;
	}
	i = getch();
	if (pendt)
		goto lt;
	if ((j = cbits(i)) == XPAR) {
		copyf++;
		tflg++;
		while (cbits(i) != '\n')
			pchar(i = getch());
		tflg = 0;
		copyf--;
		goto loop;
	}
	if (j == cc || j == c2) {
		if (j == c2)
			nb++;
		copyf++;
		while ((j = cbits(i = getch())) == ' ' || j == '\t')
			;
		ch = i;
		copyf--;
		control(getrq(), 1);
		flushi();
		goto loop;
	}
lt:
	ch = i;
	text();
	goto loop;
}


catch()
{
	done3(01);
}


kcatch()
{
	signal(SIGTERM, SIG_IGN);
	done3(01);
}


init0()
{
	eibuf = ibufp = ibuf;
	ibuf[0] = 0;
	v.nl = -1;
}


init1(a)
char	a;
{
	register char	*p;
	char	*mktemp();
	register i;

	p = mktemp("/usr/tmp/trtmpXXXXX");
	if (a == 'a')
		p = &p[9];
	if ((close(creat(p, 0600))) < 0) {
		fprintf(stderr, "troff: cannot create temp file.\n");
		exit(-1);
	}
	ibf = open(p, 2);
	unlkp = p;
	for (i = NTRTAB; --i; )
		trtab[i] = i;
	trtab[UNPAD] = ' ';
}


init2()
{
	register i, j;
	register char *p;
	tchar *t;
	extern int	block;
	extern char	*setbrk();
	extern char	*ttyname();

	ttyod = 2;
	if ((ttyp=ttyname(j=0)) != 0 || (ttyp=ttyname(j=1)) != 0 || (ttyp=ttyname(j=2)) != 0)
		;
	else 
		ttyp = "notty";
	iflg = j;
	if (ascii)
		mesg(0);
	obufp = obuf;
	ptinit();
	mchbits();
	cvtime();
	v.pid = getpid();
	olinep = oline;
	ioff = 0;
	v.hp = init = 0;
	pinchar = inchar;	/* XXX */
	v.nl = -1;
	nfo = 0;
	ifile = 0;
	copyf = raw = 0;
	level = 0;
	eibuf = sprintf(ibuf+strlen(ibuf), ".ds .T %s\n", devname);
	for (p=ibuf, t=cbuf; *t++ = *p++; )
		;
	cp = cbuf;
	eibuf = ibuf;
	ibufp = ibuf;
	nx = mflg;
	frame = stk = (struct s *)setbrk(DELTA);
	dip = &d[0];
	nxf = frame + 1;
	for (i = NEV; i--; )
		write(ibf, (char *) & block, EVS);
}


cvtime()
{
	long	tt;
	struct tm	*date;
	extern struct tm	*localtime();

	time(&tt);
	date = localtime(&tt);
	v.dy = date->tm_mday;
	v.dw = date->tm_wday + 1;
	v.yr = date->tm_year;
	v.mo = date->tm_mon + 1;
}


ctoi(s)
	register char *s;
{
	register n;

	while (*s == ' ')
		s++;
	n = 0;
	while (isdigit(*s))
		n = 10 * n + *s++ - '0';
	return n;
}


mesg(f)
int	f;
{
	static int	mode;

	if (!f) {
		stat(ttyp, cbuf);
		mode = ((struct stat *)(cbuf))->st_mode;
		chmod(ttyp, mode & ~0122);	/* turn off writing for others */
	} else {
		chmod(ttyp, mode);
	}
}


/*
 * Scaled down version of C Library printf.
 * Only %s %u %d (==%u) %o %c %x %D are recognized.
 */
#define	putchar(n)	(*pfbp++ = (n))	/* NO CHECKING! */

static char	pfbuf[NTM];
static char	*pfbp = pfbuf;
int	stderr	 = 2;	/* NOT stdio value */

/* VARARGS */
fprintf(fd, fmt, x1)
int	fd;
char	*fmt;
unsigned	x1;
{
	register c;
	register unsigned int	*adx;
	char	*s;
	register i;

	pfbp = pfbuf;
	adx = &x1;
loop:
	while ((c = *fmt++) != '%') {
		if (c == '\0') {
			if (fd == stderr)
				write(stderr, pfbuf, pfbp - pfbuf);
			else {
				*pfbp = 0;
				pfbp = pfbuf;
				while (*pfbp) {
					*obufp++ = *pfbp++;
					if (obufp >= &obuf[OBUFSZ])
						flusho();
				}
			}
			return;
		}
		putchar(c);
	}
	c = *fmt++;
	if (c == 'd') {
		i = *adx;
		if (i < 0) {
			putchar('-');
			i = -i;
		}
		printn((long)i, 10);
	} else if (c == 'u' || c == 'o' || c == 'x')
		printn((long)*adx, c == 'o' ? 8 : (c == 'x' ? 16 : 10));
	else if (c == 'c') {
		if (c > 0177 || c < 040)
			putchar('\\');
		putchar(*adx & 0177);
	} else if (c == 's') {
		s = (char *) * adx;
		while (c = *s++)
			putchar(c);
	} else if (c == 'D') {
		printn(*(long *)adx, 10);
		adx += (sizeof(long) / sizeof(int)) - 1;
	} else if (c == 'O') {
		printn(*(long *)adx, 8);
		adx += (sizeof(long) / sizeof(int)) - 1;
	}
	adx++;
	goto loop;
}


/*
 * Print an unsigned integer in base b.
 */
static printn(n, b)
long	n;
{
	register long	a;

	if (n < 0) {	/* shouldn't happen */
		putchar('-');
		n = -n;
	}
	if (a = n / b)
		printn(a, b);
	putchar("0123456789ABCDEF"[(int)(n%b)]);
}

/* scaled down version of library sprintf */
/* same limits as fprintf */
/* returns pointer to \0 that ends the string */

/* VARARGS */
char *sprintf(str, fmt, x1)
	char	*str;
	char	*fmt;
	unsigned	x1;
{
	register c;
	char *sprintn();
	register unsigned int	*adx;
	char	*s;
	register i;

	adx = &x1;
loop:
	while ((c = *fmt++) != '%') {
		if (c == '\0') {
			*str = 0;
			return str;
		}
		*str++ = c;
	}
	c = *fmt++;
	if (c == 'd') {
		i = *adx;
		if (i < 0) {
			*str++ = '-';
			i = -i;
		}
		str = sprintn(str, (long)i, 10);
	} else if (c == 'u' || c == 'o' || c == 'x')
		str = sprintn(str, (long)*adx, c == 'o' ? 8 : (c == 'x' ? 16 : 10));
	else if (c == 'c') {
		if (c > 0177 || c < 040)
			*str++ = '\\';
		*str++ = *adx & 0177;
	} else if (c == 's') {
		s = (char *) * adx;
		while (c = *s++)
			*str++ = c;
	} else if (c == 'D') {
		str = sprintn(str, *(long *)adx, 10);
		adx += (sizeof(long) / sizeof(int)) - 1;
	} else if (c == 'O') {
		str = sprintn(str, *(long *)adx, 8);
		adx += (sizeof(long) / sizeof(int)) - 1;
	}
	adx++;
	goto loop;
}

/*
 * Print an unsigned integer in base b.
 */
static char *sprintn(s, n, b)
	register char *s;
	register long n;
{
	register long	a;

	if (n < 0) {	/* shouldn't happen */
		*s++ = '-';
		n = -n;
	}
	if (a = n / b)
		s = sprintn(s, a, b);
	*s++ = "0123456789ABCDEF"[(int)(n%b)];
	return s;
}


control(a, b)
register int	a, b;
{
	register int	j;

	if (a == 0 || (j = findmn(a)) == -1)
		return(0);
	if (contab[j].rq & MMASK) {
		nxf->nargs = 0;
		if (b)
			collect();
		flushi();
		return(pushi((filep)contab[j].x.mx));
	} else if (b)
		return((*contab[j].x.f)(0));
	else
		return(0);
}


getrq()
{
	register i, j;

	if (((i = getach()) == 0) || ((j = getach()) == 0))
		goto rtn;
	i = PAIR(i, j);
rtn:
	return(i);
}


tchar getch()
{
	register int	k;
	tchar i, j;
	tchar setht(), setslant();

	level++;
g0:
	if (ch) {
		if (cbits(i = ch) == '\n')
			nlflg++;
		ch = 0;
		level--;
		return(i);
	}

	if (nlflg) {
		level--;
		return('\n');
	}

	if ((k = cbits(i = getch0())) != ESC) {
		if (ismot(i))
			goto g2;
		if (k == FLSS) {
			copyf++; 
			raw++;
			i = getch0();
			if (!fi)
				flss = i;
			copyf--; 
			raw--;
			goto g0;
		}
		if (k == RPT) {
			setrpt();
			goto g0;
		}
		if (!copyf) {
			if (k == 'f' && lg && !lgf) {
				i = getlg(i);
				goto g2;
			}
			if (k == fc || k == tabch || k == ldrch) {
				if ((i = setfield(k)) == 0)
					goto g0; 
				else 
					goto g2;
			}
			if (k == '\b') {
				i = makem(-width(' ' | chbits));
				goto g2;
			}
		}
		goto g2;
	}
	k = cbits(j = getch0());
	if (ismot(j)) {
		i = j;
		goto g2;
	}
	switch (k) {

	case '\n':	/* concealed newline */
		goto g0;
	case 'n':	/* number register */
		setn();
		goto g0;
	case '*':	/* string indicator */
		setstr();
		goto g0;
	case '$':	/* argument indicator */
		seta();
		goto g0;
	case '{':	/* LEFT */
		i = LEFT;
		goto gx;
	case '}':	/* RIGHT */
		i = RIGHT;
		goto gx;
	case '"':	/* comment */
		while (cbits(i = getch0()) != '\n')
			;
		goto g2;
	case ESC:	/* double backslash */
		i = eschar;
		goto gx;
	case 'e':	/* printable version of current eschar */
		i = PRESC;
		goto gx;
	case ' ':	/* unpaddable space */
		i = UNPAD;
		goto gx;
	case '|':	/* narrow space */
		i = NARSP;
		goto gx;
	case '^':	/* half of narrow space */
		i = HNARSP;
		goto gx;
	case '\'':	/* \(aa */
		i = ACUTE;
		goto gx;
	case '`':	/* \(ga */
		i = GRAVE;
		goto gx;
	case '_':	/* \(ul */
		i = UNDERLINE;
		goto gx;
	case '-':	/* current font minus */
		i = MINUS;
		goto gx;
	case '&':	/* filler */
		i = FILLER;
		goto gx;
	case 'c':	/* to be continued */
		i = CONT;
		goto gx;
	case '!':	/* transparent indicator */
		i = XPAR;
		goto gx;
	case 't':	/* tab */
		i = '\t';
		goto g2;
	case 'a':	/* leader (SOH) */
		i = LEADER;
		goto g2;
	case '%':	/* ohc */
		i = OHC;
		goto g2;
	case 'g':	/* return format of a number register */
		setaf();
		goto g0;
	case '.':	/* . */
		i = '.';
gx:
		setsfbits(i, sfbits(j));
		goto g2;
	}
	if (!copyf)
		switch (k) {

		case 'p':	/* spread */
			spread++;
			goto g0;
		case '(':	/* special char name */
			if ((i = setch()) == 0)
				goto g0;
			break;
		case 's':	/* size indicator */
			setps();
			goto g0;
		case 'H':	/* character height */
			i = setht();
			break;
		case 'S':	/* slant */
			i = setslant();
			break;
		case 'f':	/* font indicator */
			setfont(0);
			goto g0;
		case 'w':	/* width function */
			setwd();
			goto g0;
		case 'v':	/* vert mot */
			if (i = vmot())
				break;
			goto g0;
		case 'h': 	/* horiz mot */
			if (i = hmot())
				break;
			goto g0;
		case 'z':	/* zero with char */
			i = setz();
			break;
		case 'l':	/* hor line */
			setline();
			goto g0;
		case 'L':	/* vert line */
			setvline();
			goto g0;
		case 'D':	/* drawing function */
			setdraw();
			goto g0;
		case 'b':	/* bracket */
			setbra();
			goto g0;
		case 'o':	/* overstrike */
			setov();
			goto g0;
		case 'k':	/* mark hor place */
			if ((k = findr(getsn())) != -1) {
				vlist[k] = v.hp = sumhp();
			}
			goto g0;
		case '0':	/* number space */
			i = makem(width('0' | chbits));
			break;
		case 'x':	/* extra line space */
			if (i = xlss())
				break;
			goto g0;
		case 'u':	/* half em up */
		case 'r':	/* full em up */
		case 'd':	/* half em down */
			i = sethl(k);
			break;
		default:
			i = j;
		}
	else {
		ch0 = j;
		i = eschar;
	}
g2:
	if (cbits(i) == '\n') {
		nlflg++;
		v.hp = 0;
		pinchar = inchar;	/* XXX */
		if (ip == 0)
			v.cd++;	/* current input line number in this file */
	}
	if (!--level) {
		if (pinchar >= inchar + LNSIZE) {	/* XXX */
			inchar[0] = makem(sumhp());
			pinchar = &inchar[1];
		}
		*pinchar++ = i;	/* XXX */
	}
	return(i);
}


char	ifilt[32] = {
	0, 001, 002, 003, 0, 005, 006, 007, 010, 011, 012};


sumhp()	/* XXX - add up values in inchar */
{
	register int n;
	register tchar *p;

	n = 0;
	for (p = inchar; p < pinchar; p++)
		n += width(*p);
	return(n);
}


tchar getch0()
{
	register int	j;
	tchar i;

	if (ch0) {
		i = ch0; 
		ch0 = 0; 
		return(i);
	}
	if (nchar) {
		nchar--; 
		return(rchar);
	}

again:
	if (cp) {
		if ((i = *cp++) == 0) {
			cp = 0;
			goto again;
		}
	} else if (ap) {
		if ((i = *ap++) == 0) {
			ap = 0;
			goto again;
		}
	} else if (ip) {
		if (ip == -1)
			i = rdtty();
		else 
			i = rbf();
	} else {
		if (donef)
			done(0);
		if (nx || ibufp >= eibuf) {
			if (nfo)
				goto g1;
g0:
			if (nextfile()) {
				if (ip)
					goto again;
				if (ibufp < eibuf)
					goto g2;
			}
g1:
			nx = 0;
			if ((j = read(ifile, ibuf, IBUFSZ)) <= 0)
				goto g0;
			ibufp = ibuf;
			eibuf = ibuf + j;
			if (ip)
				goto again;
		}
g2:
		i = *ibufp++ & 0177;
		ioff++;
		if (i >= 040)
			goto g4; 
		else 
			i = ifilt[i];
	}
	if (raw)
		return(i);
	if ((j = cbits(i)) == IMP)
		goto again;
	if ((i == 0) && !init)
		goto again;
g4:
	if (copyf == 0 && (i & ~BMASK) == 0 && !iscontrol(cbits(i)))
		i |= chbits;
	if (cbits(i) == eschar)
		setcbits(i, ESC);
	return(i);
}


nextfile()
{
	register char	*p;

n0:
	if (ifile)
		close(ifile);
	if (nx) {
		p = nextf;
		if (*p != 0)
			goto n1;
	}
	if (ifi > 0) {
		if (popf())
			goto n0; /* popf error */
		return(1); /* popf ok */
	}
	if (rargc-- <= 0) {
		goto n2;
	}
	p = (argp++)[0];
n1:
	if ((p[0] == '-') && (p[1] == 0)) {
		ifile = 0;
	} else if ((ifile = open(p, 0)) < 0) {
		fprintf(stderr, "troff: cannot open %s\n", p);
		nfo -= mflg;
		done(02);
	}
	nfo++;
	v.cd = 0;
	ioff = 0;
	return(0);
n2:
	if ((nfo -= mflg) && !stdi)
		done(0);
	nfo++;
	v.cd = ifile = stdi = mflg = 0;
	ioff = 0;
	return(0);
}


popf()
{
	register i;
	register char	*p, *q;
	extern char	*ttyname();

	ioff = offl[--ifi];
	ip = ipl[ifi];
	if ((ifile = ifl[ifi]) == 0) {
		p = xbuf;
		q = ibuf;
		ibufp = xbufp;
		eibuf = xeibuf;
		while (q < eibuf)
			*q++ = *p++;
		return(0);
	}
	if ((lseek(ifile, (long)(ioff & ~(IBUFSZ - 1)), 0) < 0) || ((i = read(ifile, ibuf, IBUFSZ)) < 0))
		return(1);
	eibuf = ibuf + i;
	ibufp = ibuf;
	if (ttyname(ifile) == 0)
		if ((ibufp = ibuf + (int)(ioff & (IBUFSZ - 1))) >= eibuf)
			return(1);
	return(0);
}


flushi()
{
	if (nflush)
		return;
	ch = 0;
	if (cbits(ch0) == '\n')
		nlflg++;
	ch0 = 0;
	copyf++;
	while (!nlflg) {
		if (donef && (frame == stk))
			break;
		getch();
	}
	copyf--;
	v.hp = 0;
	pinchar = inchar;	/* XXX */
}


getach()
{
	tchar i;
	register j;

	lgf++;
	j = cbits(i = getch());
	if (ismot(i) || j == ' ' || j == '\n' || j & 0200) {
		ch = i;
		j = 0;
	}
	lgf--;
	return(j & 0177);
}


casenx()
{
	lgf++;
	skip();
	getname();
	nx++;
	nextfile();
	nlflg++;
	ip = 0;
	ap = 0;
	nchar = pendt = 0;
	frame = stk;
	nxf = frame + 1;
}


getname()
{
	register int	j, k;
	tchar i;

	lgf++;
	for (k = 0; k < (NS - 1); k++) {
		if (((j = cbits(i = getch())) <= ' ') || (j > 0176))
			break;
		nextf[k] = j;
	}
	nextf[k] = 0;
	ch = i;
	lgf--;
	return(nextf[0]);
}


caseso()
{
	register i;
	register char	*p, *q;

	lgf++;
	nextf[0] = 0;
	if (skip() || !getname() || ((i = open(nextf, 0)) < 0) || (ifi >= NSO)) {
		fprintf(stderr, "troff: can't open file %s\n", nextf);
		done(02);
	}
	flushi();
	ifl[ifi] = ifile;
	ifile = i;
	offl[ifi] = ioff;
	ioff = 0;
	ipl[ifi] = ip;
	ip = 0;
	nx++;
	nflush++;
	if (!ifl[ifi++]) {
		p = ibuf;
		q = xbuf;
		xbufp = ibufp;
		xeibuf = eibuf;
		while (p < eibuf)
			*q++ = *p++;
	}
}


casecf()
{	/* copy file without change */
#ifndef NROFF
	int	fd, n;
	char	buf[512];
	extern int	un, hpos, esc, po;

	nextf[0] = 0;
	if (skip() || !getname() || (fd = open(nextf, 0)) < 0) {
		fprintf(stderr, "troff: can't open file %s\n", nextf);
		done(02);
	}
	tbreak();
	/* make it into a clean state, be sure that everything is out */
	hpos = po;
	esc = un;
	ptesc();
	ptlead();
	ptps();
	ptfont();
	flusho();
	while ((n = read(fd, buf, 512)) > 0)
		write(ptid, buf, n);
	close(fd);
#endif
}


casesy()
{	/* call system */
	char	sybuf[NTM];
	int	i;

	lgf++;
	copyf++;
	skip();
	for (i = 0; i < NTM - 2; i++)
		if ((sybuf[i] = getch()) == '\n')
			break;
	sybuf[i] = 0;
	system(sybuf);
	copyf--;
}


getpn(a)
	register char *a;
{
	register int n, neg;

	if (*a == 0)
		return;
	neg = 0;
	for ( ; *a; a++)
		switch (*a) {
		case '+':
		case ',':
			continue;
		case '-':
			neg = 1;
			continue;
		default:
			n = 0;
			if (isdigit(*a)) {
				do
					n = 10 * n + *a++ - '0';
				while (isdigit(*a));
				a--;
			} else
				n = 9999;
			*pnp++ = neg ? -n : n;
			neg = 0;
			if (pnp >= &pnlist[NPN-2]) {
				fprintf(stderr, "troff: too many page numbers\n");
				done3(-3);
			}
		}
	if (neg)
		*pnp++ = -9999;
	*pnp = -32767;
	print = 0;
	pnp = pnlist;
	if (*pnp != -32767)
		chkpn();
}


setrpt()
{
	tchar i, j;

	copyf++;
	raw++;
	i = getch0();
	copyf--;
	raw--;
	if (i < 0 || cbits(j = getch0()) == RPT)
		return;
	rchar = j;
	nchar = i & BMASK;
}
