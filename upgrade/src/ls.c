#

/*
 * ls - list file or directory
 *
 * Modified by Bill Joy UCB May/August 1977
 *
 * This version of ls is designed for graphic terminals and to
 * list directories with lots of files in them compactly.
 * It supports three variants for listings:
 *
 *	1) Columnar output.
 *	2) Stream output.
 *	3) Old one per line format.
 *
 * Columnar output is the default.
 * If, however, the standard output is not a teletype, the default
 * is one-per-line.
 *
 * With columnar output, the items are sorted down the columns.
 * We use columns only for a directory we are interpreting.
 * Thus, in particular, we do not use columns for
 *
 *	ls /usr/bin/p*
 *
 * This version of ls also prints non-printing characters as '?' if
 * the standard output is a teletype.
 *
 * Flags relating to these and other new features are:
 *
 *	-m	force stream output.
 *
 *	-1	force one entry per line, e.g. to a teletype
 *
 *	-q	force non-printings to be '?'s, e.g. to a file
 *
 *	-c	force columnar output, e.g. into a file
 *
 *	-n	like -l, but user/group id's in decimal rather than
 *		looking in /etc/passwd to save time
 */

struct {
	int	fdes;
	int	nleft;
	char	*nextc;
	char	buff[512];
} inf, obuf;

struct ibuf {
	int	idev;
	int	inum;
	int	iflags;
	char	inl;
	char	iuid;
	char	igid;
	char	isize0;
	int	isize;
	int	iaddr[8];
	char	*iatime[2];
	char	*imtime[2];
};

struct lbuf {
	char	lname[15];
	int	lnum;
	int	lflags;
	char	lnl;
	char	luid;
	char	lgid;
	char	lsize0;
	int	lsize;
	char	*lmtime[2];
	unsigned int lquot[2];
};

struct lbufx {
	char	*namep;
};

int	aflg, dflg, lflg, sflg, tflg, uflg, iflg, fflg, nflg, Aflg;
int	cflg, qflg, across;
int	nopad;
int	rflg	1;
char	*year;
int	flags;
int	uidfil	-1;
int	lastuid	-1;
char	tbuf[16];
int	tblocks;
int	statreq;
extern	end;
struct	lbuf	*lastp	&end;
struct	lbuf	*rlastp	&end;
char	*dotp	".";

#define	IFMT	060000
#define	DIR	0100000
#define	CHR	020000
#define	BLK	040000
#define	ISARG	01000
#define	LARGE	010000
#define	STXT	010000
#define	SUID	04000
#define	SGID	02000
#define	ROWN	0400
#define	WOWN	0200
#define	XOWN	0100
#define	RGRP	040
#define	WGRP	020
#define	XGRP	010
#define	ROTH	04
#define	WOTH	02
#define	XOTH	01
#define	RSTXT	01000

int	colwidth 16;
int	outcol;

main(argc, argv)
	int argc;
	char **argv;
{
	char *cp;
	int i, j;
	register struct lbuf *ep;
	register struct lbuf *slastp;
	struct lbuf lb;
	int t;
	int compar();

	obuf.fdes = 1;
	qflg = gtty(1, &obuf.buff) == 0;
	/*
	 * If the standard output is not a teletype,
	 * then we default to one-per-line format
	 * otherwise decide between stream and
	 * columnar based on our name.
	 */
	if (qflg) {
		cflg = 1;
		for (cp = argv[0]; cp[0] && cp[1]; cp++)
			continue;
		/*
		 * Name ends in l => stream
		 */
		if (cp[0] == 'l')
			nopad = 1, cflg = 0;
		/*
		 * ... if doesn't end in l or s ==> columns sorted across
		 */
		else if (cp[0] != 's')
			across = 1;
	}
	time(lb.lmtime);
	year = lb.lmtime[0] - 245; /* 6 months ago */
	while (--argc > 0 && *argv[1] == '-') {
		argv++;
		while (*++*argv) switch (**argv) {
		/*
		 * c - force columnar output
		 */
		case 'c':
			cflg = 1;
			nopad = 0;
			continue;
		/*
		 * m - force stream output
		 */
		case 'm':
			cflg = 0;
			nopad = 1;
			continue;
		/*
		 * x - force sort across
		 */
		case 'x':
			across = 1;
			nopad = 0;
			cflg = 1;
			continue;
		/*
		 * q - force ?'s in output
		 */
		case 'q':
			qflg = 1;
			continue;
		/*
		 * 1 - force 1/line in output
		 */
		case '1':
			cflg = 0;
			nopad = 0;
			continue;
		/* STANDARD FLAGS */
		case 'a':
			aflg++;
			continue;
		case 'A':
			Aflg++;
			continue;

		case 's':
			colwidth = 24;
			sflg++;
			statreq++;
			continue;

		case 'd':
			dflg++;
			continue;

		/*
		 * n - don't look in password file
		 */
		case 'n':
			nflg++;
		case 'l':
			lflg++;
			statreq++;
			continue;

		case 'r':
			rflg = -1;
			continue;

		case 't':
			tflg++;
			statreq++;
			continue;

		case 'u':
			uflg++;
			continue;

		case 'i':
			colwidth = 24;
			iflg++;
			continue;

		case 'f':
			fflg++;
			continue;

		default:
			continue;
		}
	}
	if (fflg) {
		aflg++;
		lflg = 0;
		sflg = 0;
		tflg = 0;
		statreq = 0;
	}
	if(lflg) {
		cflg = 0;
		t = "/etc/passwd";
		nopad = 0;
		colwidth = 70;
		uidfil = open(t, 0);
	}
	if (argc==0) {
		argc++;
		argv = &dotp - 1;
	}
	for (i=0; i < argc; i++) {
		if ((ep = gstat(*++argv, 1))==0)
			continue;
		ep->namep = *argv;
		ep->lflags =| ISARG;
	}
	qsort(&end, lastp - &end, sizeof *lastp, compar);
	slastp = lastp;
	for (ep = &end; ep<slastp; ep++) {
		if (ep->lflags&DIR && dflg==0 || fflg) {
			if (argc>1) {
				printf("\n%s:\n", ep->namep);
			}
			lastp = slastp;
			readdir(ep->namep);
			if (fflg==0)
				qsort(slastp,lastp - slastp,sizeof *lastp,compar);
			if (statreq) {
				printf("total %s", locv(0, tblocks));
			}
			pem(slastp, lastp);
			newline();
		} else 
			pentry(ep);
	}
	if (outcol)
		putc('\n', &obuf);
	fflush(&obuf);
	exit(0);
}

pem(slp, lp)
	register struct lbuf *slp, *lp;
{
	int ncols, nrows, row, col;
	register struct lbuf *ep;

	ncols = 80 / colwidth;
	if (ncols == 1 || cflg == 0) {
		for (ep = slp; ep < lp; ep++)
			pentry(ep);
		return;
	}
	if (across) {
		for (ep = slp; ep < lp; ep++)
			pentry(ep);
		return;
	}
	if (statreq)
		slp--;
	nrows = (lp - slp - 1) / ncols + 1;
	for (row = 0; row < nrows; row++) {
		col = row == 0 && statreq;
		for (; col < ncols; col++) {
			ep = slp + (nrows * col) + row;
			if (ep < lp)
				pentry(ep);
		}
		if (outcol)
			printf("\n");
	}
}

putchar(c)
	char c;
{

	switch (c) {
		case '\t':
			outcol = (outcol + 8) &~ 7;
			break;
		case '\n':
			outcol = 0;
			break;
		default:
			if (qflg && (c < ' ' || c >= 0177))
				c = '?';
			outcol++;
			break;
	}
	putc(c, &obuf);
}

newline()
{
	if (outcol)
		putc('\n', &obuf);
	outcol = 0;
}

column()
{
	register int i, j;

	if (outcol == 0)
		return;
	if (nopad) {
		putc(',', &obuf);
		outcol++;
		if (outcol + colwidth + 2 > 80) {
			putc('\n', &obuf);
			outcol = 0;
			return;
		}
		putc(' ', &obuf);
		outcol++;
		return;
	}
	if (cflg == 0) {
		putc('\n', &obuf);
		return;
	}
	if ((outcol / colwidth + 2) * colwidth > 80) {
		putc('\n', &obuf);
		outcol = 0;
		return;
	}
	do {
		i = colwidth - outcol % colwidth;
		j = (outcol + 8) &~ 7;
		if (j - outcol > i)
			break;
		putc('\t', &obuf);
		outcol = j;
	} while (outcol % colwidth);
	while (outcol % colwidth) {
		outcol++;
		putc(' ', &obuf);
	}
}

pentry(ap)
struct lbuf *ap;
{
	struct { char dminor, dmajor;};
	register t;
	register struct lbuf *p;
	register char *cp;

	p = ap;
	if (p->lnum == -1)
		return;
	column();
	if (iflg)
		if (nopad && !lflg)
			printf("%d ", p->lnum);
		else
			printf("%4d ", p->lnum);
	if (lflg) {
		pmode(p);
		printf("%3d ", p->lnl&0377);
		t = /* p->lgid<<8 | */ (p->luid&0377);
		if (nflg == 0 && getpw(t, tbuf)==0) {
			char *cp = tbuf;
			while (*cp && *cp != ':') cp++; *cp = 0;
			printf("%-8.8s", tbuf);
		} else
			printf("%8d", t);
		if (p->lflags & (BLK|CHR)) {
			if (p->lflags&CHR && p->lsize == -1)
				if (p->lquot[0] < 10000 && p->lquot[1] < 10000)
					printf("%4d/%4d",
						p->lquot[0], p->lquot[1]);
				else
					printf("%u/%u",
						p->lquot[0], p->lquot[1]);
			else
				printf("%4d,%4d", p->lsize.dmajor&0477,
				    p->lsize.dminor&0377);
		} else
			printf("%9s", locv(p->lsize0&0377, p->lsize));

	}
	if (sflg) {
		t = nblock(p->lsize0&0377, p->lsize);
		if (nopad && !lflg)
			printf("%s ", locv(0, t));
		else
			printf("%4s ", locv(0, t));
	}
	if (lflg) {
		if (!sflg)
			putchar(' ');
		cp = ctime(p->lmtime);
		if(p->lmtime[0] < year)
			printf("%-7.7s %-4.4s ", cp+4, cp+20); else
			printf("%-12.12s ", cp+4);
	}
	if (p->lflags&ISARG)
		printf("%s", p->namep);
	else
		printf("%.14s", p->lname);
}

nblock(size0, size)
int size0, size;
{
	register int n;

	n = ldiv(size0&0377, size, 512);
	if (size&0777)
		n++;
	if (n>8)
		n =+ (n+255)/256;
	return(n);
}

int	m0[] { 3, DIR, 'd', BLK, 'b', CHR, 'c', '-'};
int	m1[] { 1, ROWN, 'r', '-' };
int	m2[] { 1, WOWN, 'w', '-' };
int	m3[] { 2, SUID, 's', XOWN, 'x', '-' };
int	m1a[] { 1, RGRP, 'r', '-' };
int	m1b[] { 1, WGRP, 'w', '-' };
int	m1c[] { 2, SGID, 's', XGRP, 'x', '-' };
int	m4[] { 1, ROTH, 'r', '-' };
int	m5[] { 1, WOTH, 'w', '-' };
int	m6[] { 2, STXT, 't', XOTH, 'x', '-' };

int	*m[] { m0, m1, m2, m3, m1a, m1b, m1c, m4, m5, m6};

pmode(ptr)
char *ptr;
{
	register int **mp;
	register struct lbuf *p;

	p = ptr;
	flags = p->lflags;
	if (flags&CHR && p->lsize == -1)
		putchar('q');
	else
		select(m[0]);
	for (mp = &m[1]; mp < &m[10];)
		select(*mp++);
}

select(pairp)
int *pairp;
{
	register int n, *ap;

	ap = pairp;
	n = *ap++;
	while (--n>=0 && (flags&*ap++)==0)
		ap++;
	putchar(*ap);
}

makename(dir, file)
char *dir, *file;
{
	static char dfile[100];
	register char *dp, *fp;
	register int i;

	dp = dfile;
	fp = dir;
	while (*fp)
		*dp++ = *fp++;
	*dp++ = '/';
	fp = file;
	for (i=0; i<14; i++)
		*dp++ = *fp++;
	*dp = 0;
	return(dfile);
}

readdir(dir)
char *dir;
{
	static struct {
		int	dinode;
		char	dname[14];
	} dentry;
	register char *p;
	register int j;
	register struct lbuf *ep;

	if (fopen(dir, &inf) < 0) {
		newline();
		printf("%s unreadable\n", dir);
		return;
	}
	tblocks = 0;
	for(;;) {
		p = &dentry;
		for (j=0; j<16; j++)
			*p++ = getc(&inf);
		if (dentry.dinode==0
		 || aflg==0 && dentry.dname[0]=='.' && (
			!Aflg ||
		   dentry.dname[1] == 0 || (dentry.dname[1] == '.'
		   && dentry.dname[2] == 0)))
			continue;
		if (dentry.dinode == -1)
			break;
		ep = gstat(makename(dir, dentry.dname), 0);
		if (ep->lnum != -1)
			ep->lnum = dentry.dinode;
		for (j=0; j<14; j++)
			ep->lname[j] = dentry.dname[j];
	}
	close(inf.fdes);
}

gstat(file, argfl)
char *file;
{
	struct ibuf statb;
	register struct lbuf *rep;

	if (lastp+1 >= rlastp) {
		sbrk(512);
		rlastp.idev =+ 512;
	}
	rep = lastp;
	lastp++;
	rep->lflags = 0;
	rep->lnum = 0;
	if (argfl || statreq) {
		if (stat(file, &statb)<0) {
			newline();
			printf("%s not found\n", file);
			statb.inum = -1;
			statb.isize0 = 0;
			statb.isize = 0;
			statb.iflags = 0;
			if (argfl) {
				lastp--;
				return(0);
			}
		}
		rep->lnum = statb.inum;
		statb.iflags =& ~DIR;
		if ((statb.iflags&IFMT) == 060000) {
			statb.iflags =& ~020000;
		} else if ((statb.iflags&IFMT)==040000) {
			statb.iflags =& ~IFMT;
			statb.iflags =| DIR;
		}
		statb.iflags =& ~ LARGE;
		if (statb.iflags & RSTXT)
			statb.iflags =| STXT;
		statb.iflags =& ~ RSTXT;
		rep->lflags = statb.iflags;
		rep->luid = statb.iuid;
		rep->lgid = statb.igid;
		rep->lnl = statb.inl;
		rep->lsize0 = statb.isize0;
		rep->lsize = statb.isize;
		if (rep->lflags & (BLK|CHR) && lflg) {
			rep->lsize = statb.iaddr[0];
			rep->lquot[0] = statb.iaddr[1];
			rep->lquot[1] = statb.iaddr[2];
		}
		rep->lmtime[0] = statb.imtime[0];
		rep->lmtime[1] = statb.imtime[1];
		if(uflg) {
			rep->lmtime[0] = statb.iatime[0];
			rep->lmtime[1] = statb.iatime[1];
		}
		tblocks =+ nblock(statb.isize0, statb.isize);
	}
	return(rep);
}

compar(ap1, ap2)
struct lbuf *ap1, *ap2;
{
	register struct lbuf *p1, *p2;
	register int i;
	int j;
	struct { char *charp;};

	p1 = ap1;
	p2 = ap2;
	if (dflg==0) {
		if ((p1->lflags&(DIR|ISARG)) == (DIR|ISARG)) {
			if ((p2->lflags&(DIR|ISARG)) != (DIR|ISARG))
				return(1);
		} else {
			if ((p2->lflags&(DIR|ISARG)) == (DIR|ISARG))
				return(-1);
		}
	}
	if (tflg) {
		i = 0;
		if (p2->lmtime[0] > p1->lmtime[0])
			i++;
		else if (p2->lmtime[0] < p1->lmtime[0])
			i--;
		else if (p2->lmtime[1] > p1->lmtime[1])
			i++;
		else if (p2->lmtime[1] < p1->lmtime[1])
			i--;
		return(i*rflg);
	}
	if (p1->lflags&ISARG)
		p1 = p1->namep;
	else
		p1 = p1->lname;
	if (p2->lflags&ISARG)
		p2 = p2->namep;
	else
		p2 = p2->lname;
	for (;;)
		if ((j = *p1.charp++ - *p2.charp++) || p1.charp[-1]==0)
			return(rflg*j);
	return(0);
}

strcpy(to, from)
	char *to, *from;
{

	while (*to++ = *from++)
		continue;
}

strcat(to, from)
	char *to, *from;
{

	while (*to)
		to++;
	strcpy(to, from);
}
