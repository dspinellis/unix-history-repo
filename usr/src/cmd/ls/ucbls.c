#define UCB			/* Controls output format for -F */
/* #define	UCB_PWHASH	/* If have hashed password file */

/*
 * ls - list file or directory
 *
 * Modified by Bill Joy UCB May/August 1977
 * Modified by Dave Presotto BTL Feb/80
 * Modified by Bill Joy and Mark Horton Summer 1980
 *
 * this version of ls is designed for graphic terminals and to
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
 *	-C	force columnar output, e.g. into a file
 *
 *	-n	like -l, but user/group id's in decimal rather than
 *		looking in /etc/passwd to save time
 *
 *	-F	turns on the "flagging" of executables and directories
 *
 *	-R	causes ls to recurse through the branches of the subtree
 *		ala find
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <grp.h>
#include <utmp.h>

struct	utmp	utmp;
#define NMAX	(sizeof utmp.ut_name)

#define MAXFILEWIDTH 14
#define NFILES	1024
FILE	*pwdf, *dirf;

struct lbuf {
	union {
		char	lname[15];
		char	*namep;
	} ln;
	char	ltype;
	ino_t	lnum;
	short	lflags;
	short	lnl;
	short	luid;
	short	lgid;
	long	lsize;
	long	lmtime;
};

struct dchain {
	char *dc_name;		/* the path name */
	struct dchain *dc_next; /* the next directory on the chain */
};

struct dchain *dfirst;		/* the start of the directory chain */
struct dchain *cdfirst;		/* the start of the current directory chain */
struct dchain *dtemp;		/* temporary used when linking */
char *curdir;			/* the current directory */

int	aflg, bflg, dflg, lflg, sflg, tflg, uflg, iflg, fflg, gflg, cflg;
int	Aflg, nflg, qflg, Fflg, Rflg, across, Cflg;
int	nopad;
int	tabflg;
int	rflg	= 1;
long	year;
int	flags;
long	tblocks;
int	statreq;
int	xtraent;		/* for those switches which print out a total */
struct	lbuf	*flist[NFILES];
struct	lbuf	**lastp = flist;
struct	lbuf	**firstp = flist;
char	*dotp	= ".";

char	*makename();
struct	lbuf *gstat();
char	*ctime();
long	nblock();
char	*getname();

#define ISARG	0100000
int	colwidth;
int	filewidth;
int	fixedwidth;
int	outcol;

char	obuf[BUFSIZ];

main(argc, argv)
int argc;
char *argv[];
{
#include <sgtty.h>

	int i, width;
	register struct lbuf *ep;
	register struct lbuf **slastp;
	struct lbuf **epp;
	struct lbuf lb;
	char *t;
	char *cp;
	int compar();
	struct sgttyb sgbuf;

	Fflg = 0;
	tabflg = 0;
	Aflg = getuid() == 0;
	setbuf(stdout, obuf);
	lb.lmtime = time((long *) 0);
	year = lb.lmtime - 6L*30L*24L*60L*60L; /* 6 months ago */
	qflg = gtty(1, &sgbuf) == 0;

	/* guarantee at least on column width */
	fixedwidth = 2;

	/*
	 * If the standard output is not a teletype,
	 * then we default to one-per-line format
	 * otherwise decide between stream and
	 * columnar based on our name.
	 */
	if (qflg) {
		Cflg = 1;
		if ((sgbuf.sg_flags & XTABS) == 0)
			tabflg++;
		for (cp = argv[0]; cp[0] && cp[1]; cp++)
			continue;
		/*
		 * Certain kinds of links (l, ll, lr, lf, lx) cause some
		 * various options to be turned on.
		 */
		switch (cp[0]) {
		case 'l':
			if (cp[-1] == 'l') {
				/* ll => -l */
				lflg = 1;
				statreq++;
				xtraent++;
			} else {
				/* l => -m */
				nopad = 1;
				Cflg = 0;
			}
			break;
		case 'x':	/* lx => -x */
			across = 1;
			break;
		case 'f':	/* lf => -F */
			Fflg = 1;
			break;
		case 'r':	/* lr => -R */
			Rflg = 1;
			break;
		}
	} else {
		tabflg++;
	}

	while (--argc > 0 && *argv[1] == '-') {
		argv++;
		while (*++*argv) switch (**argv) {
		/*
		 * C - force columnar output
		 */
		case 'C':
			Cflg = 1;
			nopad = 0;
			continue;
		/*
		 * m - force stream output
		 */
		case 'm':
			Cflg = 0;
			nopad = 1;
			continue;
		/*
		 * x - force sort across
		 */
		case 'x':
			across = 1;
			nopad = 0;
			Cflg = 1;
			continue;
		/*
		 * q - force ?'s in output
		 */
		case 'q':
			qflg = 1;
			bflg = 0;
			continue;
		/*
		 * b - force octal value in output
		 */
		case 'b':
			bflg = 1;
			qflg = 0;
			continue;
		/*
		 * 1 - force 1/line in output
		 */
		case '1':
			Cflg = 0;
			nopad = 0;
			continue;
		/* STANDARD FLAGS */
		case 'a':
			aflg++;
			continue;

		case 'A':
			Aflg = !Aflg;
			continue;

		case 'c':
			cflg++;
			continue;

		case 's':
			fixedwidth += 5;
			sflg++;
			statreq++;
			xtraent++;
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
			xtraent++;
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
			fixedwidth += 6;
			iflg++;
			continue;

		case 'f':
			fflg++;
			continue;

		case 'g':
			gflg++;
			continue;

		case 'F':
			Fflg++;
			continue;

		case 'R':
			Rflg++;
			continue;

		default:
			fprintf (stderr, "usage: ls [-1ACFRabcdfgilmnqrstux] [files]\n");
			exit(1);
		}
	}
	if (Fflg)
#ifdef UCB
		fixedwidth++;
#else
		fixedwidth += 2;
#endif
	if (fflg) {
		aflg++;
		lflg = 0;
		sflg = 0;
		tflg = 0;
		statreq = 0;
		xtraent = 0;
	}
	if(lflg) {
		Cflg = 0;
		t = "/etc/passwd";
		if (gflg)
			t = "/etc/group";
		nopad = 0;
		fixedwidth = 70;
		pwdf = fopen(t, "r");
	}
	if (argc==0) {
		argc++;
		argv = &dotp - 1;
	}
	for (i=0; i < argc; i++) {
		argv++;
		if (Cflg) {
			width = strlen (*argv);
			if (width > filewidth)
				filewidth = width;
		}
		if ((ep = gstat(*argv, 1))==NULL)
			continue;
		ep->ln.namep = *argv;
		ep->lflags |= ISARG;
	}
	if (!Cflg)
		filewidth = MAXFILEWIDTH;
	else
	colwidth = fixedwidth + filewidth;
	qsort(firstp, lastp - firstp, sizeof *lastp, compar);
	slastp = lastp;
	/* For each argument user typed */
	for (epp=firstp; epp<slastp; epp++) {
		ep = *epp;
		if (ep->ltype=='d' && dflg==0 || fflg)
			pdirectory(ep->ln.namep, (argc>1), slastp);
		else 
			pentry(ep);

		/* -R: print subdirectories found */
		while (dfirst || cdfirst) {
			/* Place direct subdirs on front in right order */
			while (cdfirst) {
				/* reverse cdfirst onto front of dfirst */
				dtemp = cdfirst;
				cdfirst = cdfirst -> dc_next;
				dtemp -> dc_next = dfirst;
				dfirst = dtemp;
			}
			/* take off first dir on dfirst & print it */
			dtemp = dfirst;
			dfirst = dfirst->dc_next;
			pdirectory (dtemp->dc_name, 1, firstp);
			cfree (dtemp->dc_name);
			cfree (dtemp);
		}
	}
	if (outcol)
		putc('\n', stdout);
	fflush(stdout);
}

/*
 * pdirectory: print the directory name, labelling it if title is
 * nonzero, using lp as the place to start reading in the dir.
 */
pdirectory (name, title, lp)
char *name;
int title;
struct lbuf **lp;
{
	register struct dchain *dp;
	register struct lbuf *ap;
	register char *pname;
	struct lbuf **app;

	filewidth = 0;
	curdir = name;
	if (title)
		printf("\n%s:\n", name);
	lastp = lp;
	readdir(name);
	if (!Cflg)
		filewidth = MAXFILEWIDTH;
	colwidth = fixedwidth + filewidth;
#ifdef notdef
	/* Taken out because it appears this is done below in pem. */
	if (tabflg) {
		if (colwidth <= 8)
			colwidth = 8;
		else
			if (colwidth <= 16)
				colwidth = 16;
	}
#endif
	if (fflg==0)
		qsort(lp,lastp - lp,sizeof *lastp,compar);
	if (Rflg) for (app=lastp-1; app>=lp; app--) {
		ap = *app;
		if (ap->ltype == 'd' && strcmp(ap->ln.lname, ".") &&
				strcmp(ap->ln.lname, "..")) {
			dp = (struct dchain *) calloc(1, sizeof(struct dchain));
			pname = makename (curdir, ap->ln.lname);
			dp->dc_name = (char *) calloc(1, strlen(pname)+1);
			strcpy(dp->dc_name, pname);
			dp -> dc_next = dfirst;
			dfirst = dp;
		}
	}
	if (lflg || sflg)
		printf("total %D", tblocks);
	pem(lp, lastp);
	newline();
}

/*
 * pem: print 'em.  Print a list of files (e.g. a directory) bounded
 * by slp and lp.
 */
pem(slp, lp)
	register struct lbuf **slp, **lp;
{
	int ncols, nrows, row, col;
	register struct lbuf **ep;

	if (tabflg) {
		if (colwidth <= 9)
			colwidth = 8;
		else
			if (colwidth <= 17)
				colwidth = 16;
	}
	ncols = 80 / colwidth;
	if (ncols == 1 || Cflg == 0) {
		for (ep = slp; ep < lp; ep++)
			pentry(*ep);
		return;
	}
	if (across) {
		for (ep = slp; ep < lp; ep++)
			pentry(*ep);
		return;
	}
	if (xtraent)
		slp--;
	nrows = (lp - slp - 1) / ncols + 1;
	for (row = 0; row < nrows; row++) {
		col = row == 0 && xtraent;
		for (; col < ncols; col++) {
			ep = slp + (nrows * col) + row;
			if (ep < lp)
				pentry(*ep);
		}
		if (outcol)
			printf("\n");
	}
}

/*
 * pputchar: like putchar but knows how to handle control chars.
 * CAUTION: if you make ctrl chars print in ^x notation, or any
 * other notation which is wider than one character, the column
 * nature of things (such as files with 14 letter names) will be
 * messed up.  Weigh this carefully!
 */
pputchar(c)
	char c;
{
	char cc;

	switch (c) {
		case '\t':
			outcol = (outcol + 8) &~ 7;
			break;
		case '\n':
			outcol = 0;
			break;
		default:
			if (c < ' ' || c >= 0177) {
				if (qflg)
					c = '?';
				else if (bflg) {
					outcol += 3;
					putc ('\\', stdout);
					cc = '0' + (c>>6 & 07);
					putc (cc, stdout);
					cc = '0' + (c>>3 & 07);
					putc (cc, stdout);
					c = '0' + (c & 07);
				}
			}
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

/*
 * column: get to the beginning of the next column.
 */
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
	if (Cflg == 0) {
		putc('\n', stdout);
		return;
	}
	if ((outcol / colwidth + 2) * colwidth > 80) {
		putc('\n', stdout);
		outcol = 0;
		return;
	}
	if (tabflg && (colwidth <= 16)) {
		if (colwidth > 8)
			if ((outcol % 16) < 8) {
				outcol += 8 - (outcol % 8);
				putc ('\t', stdout);
			}
		outcol += 8 - (outcol % 8);
		putc ('\t', stdout);
		return;
	}
	do {
		outcol++;
		putc(' ', stdout);
	} while (outcol % colwidth);
}


/*
 * nblock: the number of 512 byte blocks a size byte file takes up.
 * (Note: the number stays 512 no matter what BUFSIZ or the filesystem uses.)
 */
long
nblock(size)
long size;
{
	return((size+511)>>9);
}

/*
 * This code handles the rwx- business.
 * You figure it out.
 */
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

/*
 * returns cat(dir, "/", file), unless dir ends in /, when it doesn't //
 */
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
	if (*(dp-1) != '/')
	*dp++ = '/';
	fp = file;
	for (i=0; i<DIRSIZ; i++)
		*dp++ = *fp++;
	*dp = 0;
	return(dfile);
}

/*
 * readdir: read in the directory whose name is dir,
 * starting at lastp.
 */
readdir(dir)
char *dir;
{
	static struct direct dentry;
	register int j, width;
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
		if (Cflg) {
			width = strlen (dentry.d_name);
			if (width > filewidth)
				filewidth = width;
		}
		ep = gstat(makename(dir, dentry.d_name), Fflg || Rflg);
		if (ep==NULL)
			continue;
		if (ep->lnum != -1)
			ep->lnum = dentry.d_ino;
		for (j=0; j<DIRSIZ; j++)
			ep->ln.lname[j] = dentry.d_name[j];
	}
	fclose(dirf);
}

/*
 * stat the given file and return an lbuf containing it.
 * argfl is nonzero if a stat is required because the file is
 * an argument, rather than having been found in a directory.
 */
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

		case S_IFMPB:
			rep->ltype = 'M';
			rep->lsize = statb.st_rdev;
			break;

		case S_IFMPC:
			rep->ltype = 'm';
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

/*
 * decide whether to print pp1 before or after pp2, based on their
 * names, various times, and the r flag.
 */
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

/*
 * print the entry pointed at by ap
 */
pentry(ap)
struct lbuf *ap;
{
	struct { char dminor, dmajor;};
	register struct lbuf *p;
	register char *cp;
	char fname[100];
	char *pname;
	struct passwd *getpwuid();
	struct passwd *pwptr;
	struct group *getgrgid();
	struct group *grptr;

	fname[0] = 0;
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
		switch (p->ltype) {
		
		case 'b':
		case 'c':
		case 'm':
		case 'M':
			if (nopad && !lflg)
				printf("%D ", 0);
			else
				printf("%4D ", 0);
			break;

		default:
			if (nopad && !lflg)
				printf("%D ", nblock(p->lsize));
			else
				printf("%4D ", nblock(p->lsize));
			break;
		}
	if (lflg) {
		pputchar(p->ltype);
		pmode(p->lflags);
		printf("%2d ", p->lnl);
		if(gflg) {
			grptr = getgrgid(p->lgid);
			if (nflg == 0 && grptr != 0)
				printf("%-8.8s", grptr->gr_name);
			else
				printf("%-8d", p->lgid);
		} else {
#ifndef UCB_PWHASH
			char *name;
			if (nflg == 0 && (name = getname(p->luid))) {
				printf("%-8.8s", name);
			}
#else
			pwptr = getpwuid(p->luid);
			if (nflg == 0 && pwptr != 0)
				printf("%-8.8s", pwptr->pw_name);
#endif
			else
				printf("%-8d", p->luid);
		}
		switch (p->ltype) {

		case 'b':
		case 'c':
		case 'm':
		case 'M':
			printf("%3d,%3d",
			    major((int)p->lsize), minor((int)p->lsize));
			break;
		default:
			printf("%7ld", p->lsize);
		}
		cp = ctime(&p->lmtime);
		if(p->lmtime < year)
			printf(" %-7.7s %-4.4s ", cp+4, cp+20); else
			printf(" %-12.12s ", cp+4);
	}
#ifndef UCB
	if (Fflg) {
	    if (p->ltype == 'd')
		strcat (fname, "[");
	    else if (p->lflags & 0111)
		strcat (fname, "*");
	    else if (!nopad)
		strcat (fname, " ");
	}
#endif
	if (p->lflags & ISARG)
	    strncat (fname, p->ln.namep, 98);
	else
	    strncat (fname, p->ln.lname, 14);
#ifndef UCB
	if (Fflg) {
	    if (p->ltype == 'd')
		strcat (fname, "]");
	    else if (!nopad)
		strcat (fname, " ");
	}
#else
	if (Fflg) {
	    if (p->ltype == 'd')
		strcat (fname, "/");
	    else if (p->lflags & 0111)
		strcat (fname, "*");
	    else if (!nopad)
		strcat (fname, " ");
	}
#endif
	printf ("%s", fname);
	free(ap);
}

/* char printf_id[] = "@(#) printf.c:2.2 6/5/79";*/

#include "varargs.h"

/*
 * This version of printf is compatible with the Version 7 C
 * printf. The differences are only minor except that this
 * printf assumes it is to print through pputchar. Version 7
 * printf is more general (and is much larger) and includes
 * provisions for floating point.
 */

#define MAXOCT	11	    /* Maximum octal digits in a long */
#define MAXINT	32767	    /* largest normal length positive integer */
#define BIG	1000000000  /* largest power of 10 less than an unsigned long */
#define MAXDIGS 10	    /* number of digits in BIG */

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

#ifndef UCB_PWHASH
#define NUID	2048

char	names[NUID][NMAX+1];

char *
getname(uid)
{
	register struct passwd *pw;
	static init;
	struct passwd *getpwent();

	if (uid >= 0 && uid < NUID && names[uid][0])
		return (&names[uid][0]);
	if (init == 2)
		return (0);
	if (init == 0)
		setpwent(), init = 1;
	while (pw = getpwent()) {
		if (pw->pw_uid < 0 || pw->pw_uid >= NUID)
			continue;
		if (names[pw->pw_uid][0])
			continue;
		strncpy(names[pw->pw_uid], pw->pw_name, NMAX);
		if (pw->pw_uid == uid)
			return (&names[uid][0]);
	}
	init = 2;
	endpwent();
	return (0);
}
#endif
