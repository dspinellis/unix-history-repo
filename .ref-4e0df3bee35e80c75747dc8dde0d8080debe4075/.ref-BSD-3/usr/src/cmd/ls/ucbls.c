/* Copyright (c) 1979 Regents of the University of California */
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

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <stdio.h>
#include <ctype.h>


#define	NFILES	1024
FILE	*pwdf, *dirf;

struct lbuf {
	union {
		char	lname[15];
		char	*namep;
	} ln;
	char	ltype;
	short	lnum;
	short	lflags;
	short	lnl;
	short	luid;
	short	lgid;
	long	lsize;
	long	lmtime;
};

int	aflg, dflg, lflg, sflg, tflg, uflg, iflg, fflg, gflg, cflg;
int	Aflg, nflg, qflg, across;
int	nopad;
char	buff[32];
int	rflg	= 1;
long	year;
int	flags;
int	lastuid	= -1;
char	tbuf[16];
long	tblocks;
int	statreq;
struct	lbuf	*flist[NFILES];
struct	lbuf	**lastp = flist;
struct	lbuf	**firstp = flist;
char	*dotp	= ".";

char	*makename();
struct	lbuf *gstat();
char	*ctime();
long	nblock();

#define	ISARG	0100000
int	colwidth = 15;
int	outcol;

char	obuf[BUFSIZ];

main(argc, argv)
char *argv[];
{
	int i;
	register struct lbuf *ep, **ep1;
	register struct lbuf **slastp;
	struct lbuf **epp;
	struct lbuf lb;
	char *t;
	char *cp;
	int compar();

	Aflg = getuid() == 0;
	setbuf(stdout, obuf);
	time(&lb.lmtime);
	year = lb.lmtime - 6L*30L*24L*60L*60L; /* 6 months ago */
	qflg = gtty(1, buff) == 0;
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
		 *
		else if (cp[0] == 'x')
			across = 1;
		 */
	}
	if (--argc > 0 && *argv[1] == '-') {
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
			Aflg = !Aflg;
			continue;

		case 's':
			colwidth += 5;
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
			colwidth += 5;
			iflg++;
			continue;

		case 'f':
			fflg++;
			continue;

		case 'g':
			gflg++;
			continue;

		default:
			continue;
		}
		argc--;
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
		if (gflg)
			t = "/etc/group";
		nopad = 0;
		colwidth = 70;
		pwdf = fopen(t, "r");
	}
	if (argc==0) {
		argc++;
		argv = &dotp - 1;
	}
	for (i=0; i < argc; i++) {
		if ((ep = gstat(*++argv, 1))==NULL)
			continue;
		ep->ln.namep = *argv;
		ep->lflags |= ISARG;
	}
	qsort(firstp, lastp - firstp, sizeof *lastp, compar);
	slastp = lastp;
	for (epp=firstp; epp<slastp; epp++) {
		ep = *epp;
		if (ep->ltype=='d' && dflg==0 || fflg) {
			if (argc>1)
				printf("\n%s:\n", ep->ln.namep);
			lastp = slastp;
			readdir(ep->ln.namep);
			if (fflg==0)
				qsort(slastp,lastp - slastp,sizeof *lastp,compar);
			if (lflg || sflg)
				printf("total %D", tblocks);
			pem(slastp, lastp);
			newline();
		} else 
			pentry(ep);
	}
	if (outcol)
		putc('\n', stdout);
	fflush(stdout);
}

pem(slp, lp)
	register struct lbuf **slp, **lp;
{
	int ncols, nrows, row, col;
	register struct lbuf **ep;

	ncols = 80 / colwidth;
	if (ncols == 1 || cflg == 0) {
		for (ep = slp; ep < lp; ep++)
			pentry(*ep);
		return;
	}
	if (across) {
		for (ep = slp; ep < lp; ep++)
			pentry(*ep);
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
				pentry(*ep);
		}
		if (outcol)
			printf("\n");
	}
}

pputchar(c)
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
	putc(c, stdout);
}

newline()
{
	if (outcol)
		putc('\n', stdout);
	outcol = 0;
}

column()
{

	if (outcol == 0)
		return;
	if (nopad) {
		putc(',', stdout);
		outcol++;
		if (outcol + colwidth + 2 > 80) {
			putc('\n', stdout);
			outcol = 0;
			return;
		}
		putc(' ', stdout);
		outcol++;
		return;
	}
	if (cflg == 0) {
		putc('\n', stdout);
		return;
	}
	if ((outcol / colwidth + 2) * colwidth > 80) {
		putc('\n', stdout);
		outcol = 0;
		return;
	}
	do {
		outcol++;
		putc(' ', stdout);
	} while (outcol % colwidth);
}


getname(uid, buf)
int uid;
char buf[];
{
	int j, c, n, i;

	if (uid==lastuid)
		return(0);
	if(pwdf == NULL)
		return(-1);
	rewind(pwdf);
	lastuid = -1;
	do {
		i = 0;
		j = 0;
		n = 0;
		while((c=fgetc(pwdf)) != '\n') {
			if (c==EOF)
				return(-1);
			if (c==':') {
				j++;
				c = '0';
			}
			if (j==0)
				buf[i++] = c;
			if (j==2)
				n = n*10 + c - '0';
		}
	} while (n != uid);
	buf[i++] = '\0';
	lastuid = uid;
	return(0);
}

long
nblock(size)
long size;
{
	return((size+511)>>9);
}

int	m1[] = { 1, S_IREAD>>0, 'r', '-' };
int	m2[] = { 1, S_IWRITE>>0, 'w', '-' };
int	m3[] = { 2, S_ISUID, 's', S_IEXEC>>0, 'x', '-' };
int	m4[] = { 1, S_IREAD>>3, 'r', '-' };
int	m5[] = { 1, S_IWRITE>>3, 'w', '-' };
int	m6[] = { 2, S_ISGID, 's', S_IEXEC>>3, 'x', '-' };
int	m7[] = { 1, S_IREAD>>6, 'r', '-' };
int	m8[] = { 1, S_IWRITE>>6, 'w', '-' };
int	m9[] = { 2, S_ISVTX, 't', S_IEXEC>>6, 'x', '-' };

int	*m[] = { m1, m2, m3, m4, m5, m6, m7, m8, m9};

pmode(aflag)
{
	register int **mp;

	flags = aflag;
	for (mp = &m[0]; mp < &m[sizeof(m)/sizeof(m[0])];)
		select(*mp++);
}

select(pairp)
register int *pairp;
{
	register int n;

	n = *pairp++;
	while (--n>=0 && (flags&*pairp++)==0)
		pairp++;
	pputchar(*pairp);
}

char *
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
	for (i=0; i<DIRSIZ; i++)
		*dp++ = *fp++;
	*dp = 0;
	return(dfile);
}

readdir(dir)
char *dir;
{
	static struct direct dentry;
	register int j;
	register struct lbuf *ep;

	if ((dirf = fopen(dir, "r")) == NULL) {
		printf("%s unreadable\n", dir);
		return;
	}
	tblocks = 0;
	for(;;) {
		if (fread(&dentry, sizeof(dentry), 1, dirf) != 1)
			break;
		if (dentry.d_ino==0 ||
			aflg==0 && dentry.d_name[0]=='.' && (
			!Aflg ||
			dentry.d_name[1]=='\0'
			|| dentry.d_name[1]=='.' && dentry.d_name[2]=='\0'))
			continue;
		ep = gstat(makename(dir, dentry.d_name), 0);
		if (ep==NULL)
			continue;
		if (ep->lnum != -1)
			ep->lnum = dentry.d_ino;
		for (j=0; j<DIRSIZ; j++)
			ep->ln.lname[j] = dentry.d_name[j];
	}
	fclose(dirf);
}

struct lbuf *
gstat(file, argfl)
char *file;
{
	struct stat statb;
	register struct lbuf *rep;
	static int nomocore;

	if (nomocore)
		return(NULL);
	rep = (struct lbuf *)malloc(sizeof(struct lbuf));
	if (rep==NULL) {
		fprintf(stderr, "ls: out of memory\n");
		nomocore = 1;
		return(NULL);
	}
	if (lastp >= &flist[NFILES]) {
		static int msg;
		lastp--;
		if (msg==0) {
			fprintf(stderr, "ls: too many files\n");
			msg++;
		}
	}
	*lastp++ = rep;
	rep->lflags = 0;
	rep->lnum = 0;
	rep->ltype = '-';
	if (argfl || statreq) {
		if (stat(file, &statb)<0) {
			printf("%s not found\n", file);
			statb.st_ino = -1;
			statb.st_size = 0;
			statb.st_mode = 0;
			if (argfl) {
				lastp--;
				return(0);
			}
		}
		rep->lnum = statb.st_ino;
		rep->lsize = statb.st_size;
		switch(statb.st_mode&S_IFMT) {

		case S_IFDIR:
			rep->ltype = 'd';
			break;

		case S_IFBLK:
			rep->ltype = 'b';
			rep->lsize = statb.st_rdev;
			break;

		case S_IFCHR:
			rep->ltype = 'c';
			rep->lsize = statb.st_rdev;
			break;
		}
		rep->lflags = statb.st_mode & ~S_IFMT;
		rep->luid = statb.st_uid;
		rep->lgid = statb.st_gid;
		rep->lnl = statb.st_nlink;
		if(uflg)
			rep->lmtime = statb.st_atime;
		else if (cflg)
			rep->lmtime = statb.st_ctime;
		else
			rep->lmtime = statb.st_mtime;
		tblocks += nblock(statb.st_size);
	}
	return(rep);
}

compar(pp1, pp2)
struct lbuf **pp1, **pp2;
{
	register struct lbuf *p1, *p2;

	p1 = *pp1;
	p2 = *pp2;
	if (dflg==0) {
		if (p1->lflags&ISARG && p1->ltype=='d') {
			if (!(p2->lflags&ISARG && p2->ltype=='d'))
				return(1);
		} else {
			if (p2->lflags&ISARG && p2->ltype=='d')
				return(-1);
		}
	}
	if (tflg) {
		if(p2->lmtime == p1->lmtime)
			return(0);
		if(p2->lmtime > p1->lmtime)
			return(rflg);
		return(-rflg);
	}
	return(rflg * strcmp(p1->lflags&ISARG? p1->ln.namep: p1->ln.lname,
				p2->lflags&ISARG? p2->ln.namep: p2->ln.lname));
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
			printf("%5d ", p->lnum);
	if (sflg)
		if (nopad && !lflg)
			printf("%D ", nblock(p->lsize));
		else
			printf("%4D ", nblock(p->lsize));
	if (lflg) {
		pputchar(p->ltype);
		pmode(p->lflags);
		printf("%2d ", p->lnl);
		t = p->luid;
		if(gflg)
			t = p->lgid;
		if (nflg == 0 && getname(t, tbuf)==0)
			printf("%-8.8s", tbuf);
		else
			printf("%-8d", t);
		if (p->ltype=='b' || p->ltype=='c')
			printf("%3d,%3d", major((int)p->lsize), minor((int)p->lsize));
		else
			printf("%7ld", p->lsize);
		cp = ctime(&p->lmtime);
		if(p->lmtime < year)
			printf(" %-7.7s %-4.4s ", cp+4, cp+20); else
			printf(" %-12.12s ", cp+4);
	}
	if (p->lflags&ISARG)
		printf("%s", p->ln.namep);
	else
		printf("%.14s", p->ln.lname);
}
/* char printf_id[] = "@(#) printf.c:2.2 6/5/79";*/
#include "varargs.h"
/* This version of printf is compatible with the Version 7 C
 * printf. The differences are only minor except that this
 * printf assumes it is to print through pputchar. Version 7
 * printf is more general (and is much larger) and includes
 * provisions for floating point.
 */
 

#define MAXOCT	11	/* Maximum octal digits in a long */
#define MAXINT	32767	/* largest normal length positive integer */
#define BIG	1000000000  /* largest power of 10 less than an unsigned long */
#define MAXDIGS 10	/* number of digits in BIG */

static int width, sign, fill;

char *b_dconv();

printf(va_alist)
	va_dcl
{
	va_list ap;
	register char *fmt;
	char fcode;
	int prec;
	int length,mask1,nbits,n;
	long int mask2, num;
	register char *bptr;
	char *ptr;
	char buf[134];

	va_start(ap);
	fmt = va_arg(ap,char *);
	for (;;) {
		/* process format string first */
		while ((fcode = *fmt++)!='%') {
			/* ordinary (non-%) character */
			if (fcode=='\0')
				return;
			pputchar(fcode);
		}
		/* length modifier: -1 for h, 1 for l, 0 for none */
		length = 0;
		/* check for a leading - sign */
		sign = 0;
		if (*fmt == '-') {
			sign++;
			fmt++;
		}
		/* a '0' may follow the - sign */
		/* this is the requested fill character */
		fill = 1;
		if (*fmt == '0') {
			fill--;
			fmt++;
		}
		
		/* Now comes a digit string which may be a '*' */
		if (*fmt == '*') {
			width = va_arg(ap, int);
			if (width < 0) {
				width = -width;
				sign = !sign;
			}
			fmt++;
		}
		else {
			width = 0;
			while (*fmt>='0' && *fmt<='9')
				width = width * 10 + (*fmt++ - '0');
		}
		
		/* maybe a decimal point followed by more digits (or '*') */
		if (*fmt=='.') {
			if (*++fmt == '*') {
				prec = va_arg(ap, int);
				fmt++;
			}
			else {
				prec = 0;
				while (*fmt>='0' && *fmt<='9')
					prec = prec * 10 + (*fmt++ - '0');
			}
		}
		else
			prec = -1;
		
		/*
		 * At this point, "sign" is nonzero if there was
		 * a sign, "fill" is 0 if there was a leading
		 * zero and 1 otherwise, "width" and "prec"
		 * contain numbers corresponding to the digit
		 * strings before and after the decimal point,
		 * respectively, and "fmt" addresses the next
		 * character after the whole mess. If there was
		 * no decimal point, "prec" will be -1.
		 */
		switch (*fmt) {
			case 'L':
			case 'l':
				length = 2;
				/* no break!! */
			case 'h':
			case 'H':
				length--;
				fmt++;
				break;
		}
		
		/*
		 * At exit from the following switch, we will
		 * emit the characters starting at "bptr" and
		 * ending at "ptr"-1, unless fcode is '\0'.
		 */
		switch (fcode = *fmt++) {
			/* process characters and strings first */
			case 'c':
				buf[0] = va_arg(ap, int);
				ptr = bptr = &buf[0];
				if (buf[0] != '\0')
					ptr++;
				break;
			case 's':
				bptr = va_arg(ap,char *);
				if (bptr==0)
					bptr = "(null pointer)";
				if (prec < 0)
					prec = MAXINT;
				for (n=0; *bptr++ && n < prec; n++) ;
				ptr = --bptr;
				bptr -= n;
				break;
			case 'O':
				length = 1;
				fcode = 'o';
				/* no break */
			case 'o':
			case 'X':
			case 'x':
				if (length > 0)
					num = va_arg(ap,long);
				else
					num = (unsigned)va_arg(ap,int);
				if (fcode=='o') {
					mask1 = 0x7;
					mask2 = 0x1fffffffL;
					nbits = 3;
				}
				else {
					mask1 = 0xf;
					mask2 = 0x0fffffffL;
					nbits = 4;
				}
				n = (num!=0);
				bptr = buf + MAXOCT + 3;
				/* shift and mask for speed */
				do
				    if (((int) num & mask1) < 10)
				        *--bptr = ((int) num & mask1) + 060;
				    else
					*--bptr = ((int) num & mask1) + 0127;
				while (num = (num >> nbits) & mask2);
				
				if (fcode=='o') {
					if (n)
						*--bptr = '0';
				}
				else
					if (!sign && fill <= 0) {
						pputchar('0');
						pputchar(fcode);
						width -= 2;
					}
					else {
						*--bptr = fcode;
						*--bptr = '0';
					}
				ptr = buf + MAXOCT + 3;
				break;
			case 'D':
			case 'U':
			case 'I':
				length = 1;
				fcode = fcode + 'a' - 'A';
				/* no break */
			case 'd':
			case 'i':
			case 'u':
				if (length > 0)
					num = va_arg(ap,long);
				else {
					n = va_arg(ap,int);
					if (fcode=='u')
						num = (unsigned) n;
					else
						num = (long) n;
				}
				if (n = (fcode != 'u' && num < 0))
					num = -num;
				/* now convert to digits */
				bptr = b_dconv(num, buf);
				if (n)
					*--bptr = '-';
				if (fill == 0)
					fill = -1;
				ptr = buf + MAXDIGS + 1;
				break;
			default:
				/* not a control character, 
				 * print it.
				 */
				ptr = bptr = &fcode;
				ptr++;
				break;
			}
			if (fcode != '\0')
				b_emit(bptr,ptr);
	}
	va_end(ap);
}

/* b_dconv converts the unsigned long integer "value" to
 * printable decimal and places it in "buffer", right-justified.
 * The value returned is the address of the first non-zero character,
 * or the address of the last character if all are zero.
 * The result is NOT null terminated, and is MAXDIGS characters long,
 * starting at buffer[1] (to allow for insertion of a sign).
 *
 * This program assumes it is running on 2's complement machine
 * with reasonable overflow treatment.
 */
char *
b_dconv(value, buffer)
	long value;
	char *buffer;
{
	register char *bp;
	register int svalue;
	int n;
	long lval;
	
	bp = buffer;
	
	/* zero is a special case */
	if (value == 0) {
		bp += MAXDIGS;
		*bp = '0';
		return(bp);
	}
	
	/* develop the leading digit of the value in "n" */
	n = 0;
	while (value < 0) {
		value -= BIG;	/* will eventually underflow */
		n++;
	}
	while ((lval = value - BIG) >= 0) {
		value = lval;
		n++;
	}
	
	/* stash it in buffer[1] to allow for a sign */
	bp[1] = n + '0';
	/*
	 * Now develop the rest of the digits. Since speed counts here,
	 * we do it in two loops. The first gets "value" down until it
	 * is no larger than MAXINT. The second one uses integer divides
	 * rather than long divides to speed it up.
	 */
	bp += MAXDIGS + 1;
	while (value > MAXINT) {
		*--bp = (int)(value % 10) + '0';
		value /= 10;
	}
	
	/* cannot lose precision */
	svalue = value;
	while (svalue > 0) {
		*--bp = (svalue % 10) + '0';
		svalue /= 10;
	}
	
	/* fill in intermediate zeroes if needed */
	if (buffer[1] != '0') {
		while (bp > buffer + 2)
			*--bp = '0';
		--bp;
	}
	return(bp);
}

/*
 * This program sends string "s" to pputchar. The character after
 * the end of "s" is given by "send". This allows the size of the
 * field to be computed; it is stored in "alen". "width" contains the
 * user specified length. If width<alen, the width will be taken to
 * be alen. "sign" is zero if the string is to be right-justified
 * in the field, nonzero if it is to be left-justified. "fill" is
 * 0 if the string is to be padded with '0', positive if it is to be
 * padded with ' ', and negative if an initial '-' should appear before
 * any padding in right-justification (to avoid printing "-3" as
 * "000-3" where "-0003" was intended).
 */
b_emit(s, send)
	register char *s;
	char *send;
{
	char cfill;
	register int alen;
	int npad;
	
	alen = send - s;
	if (alen > width)
		width = alen;
	cfill = fill>0? ' ': '0';
	
	/* we may want to print a leading '-' before anything */
	if (*s == '-' && fill < 0) {
		pputchar(*s++);
		alen--;
		width--;
	}
	npad = width - alen;
	
	/* emit any leading pad characters */
	if (!sign)
		while (--npad >= 0)
			pputchar(cfill);
			
	/* emit the string itself */
	while (--alen >= 0)
		pputchar(*s++);
		
	/* emit trailing pad characters */
	if (sign)
		while (--npad >= 0)
			pputchar(cfill);
}
