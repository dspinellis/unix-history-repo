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
static char sccsid[] = "@(#)ls.c	5.7 (Berkeley) %G%";
#endif not lint

/*
 * ls
 *
 * 4.2bsd version for symbolic links, variable length
 * directory entries, block size in the inode, etc.
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <stdio.h>
#include <sgtty.h>

#define	kbytes(size)	(((size) + 1023) / 1024)

struct afile {
	char	ftype;		/* file type, e.g. 'd', 'c', 'f' */
	ino_t	fnum;		/* inode number of file */
	short	fflags;		/* mode&~S_IFMT, perhaps ISARG */
	short	fnl;		/* number of links */
	short	fuid;		/* owner id */
	short	fgid;		/* group id */
	long	fsize;		/* file size */
	long	fblks;		/* number of blocks used */
	time_t	fmtime;		/* time (modify or access or create) */
	char	*fname;		/* file name */
	char	*flinkto;	/* symbolic link value */
};

#define ISARG	0x8000		/* extra ``mode'' */

struct subdirs {
	char	*sd_name;
	struct	subdirs *sd_next;
} *subdirs;

int	aflg, dflg, gflg, lflg, sflg, tflg, uflg, iflg, fflg, cflg, rflg = 1;
int	qflg, Aflg, Cflg, Fflg, Lflg, Rflg, Sflg;

int	usetabs;

time_t	now, sixmonthsago;

char	*dotp = ".";

struct	winsize win;
int	twidth;

struct	afile *gstat();
int	fcmp();
char	*cat(), *savestr();
char	*fmtentry();
char	*getname(), *getgroup();

char	*ctime();
char	*malloc(), *calloc(), *realloc();
char	*sprintf(), *strcpy(), *strcat();

main(argc, argv)
	int argc;
	char *argv[];
{
	int i;
	struct afile *fp0, *fplast;
	register struct afile *fp;
	struct sgttyb sgbuf;

	argc--, argv++;
	if (getuid() == 0)
		Aflg++;
	(void) time(&now); sixmonthsago = now - 6L*30L*24L*60L*60L; now += 60;
	twidth = 80;
	if (isatty(1)) {
		qflg = Cflg = 1;
		(void) gtty(1, &sgbuf);
		if (ioctl(1, TIOCGWINSZ, &win) != -1)
			twidth = (win.ws_col == 0 ? 80 : win.ws_col);
		if ((sgbuf.sg_flags & XTABS) != XTABS)
			usetabs = 1;
	} else
		usetabs = 1;
	while (argc > 0 && **argv == '-') {
		(*argv)++;
		while (**argv) switch (*(*argv)++) {

		case 'C':
			Cflg = 1; break;
		case 'q':
			qflg = 1; break;
		case '1':
			Cflg = 0; break;
		case 'a':
			aflg++; break;
		case 'A':
			Aflg++; break;
		case 'c':
			cflg++; break;
		case 'S':
			Sflg++; /* fall into... */
		case 's':
			sflg++; break;
		case 'd':
			dflg++; break;
		case 'g':
			gflg++; break;
		case 'l':
			lflg++; break;
		case 'r':
			rflg = -1; break;
		case 't':
			tflg++; break;
		case 'u':
			uflg++; break;
		case 'i':
			iflg++; break;
		case 'f':
			fflg++; break;
		case 'L':
			Lflg++; break;
		case 'F':
		        Fflg++; break;
		case 'R':
		        Rflg++; break;
		}
		argc--, argv++;
	}
	if (fflg) { 
		aflg++; Sflg = 0; tflg = 0; /* -f: only turn off sort flags */
	}
	if (lflg)
		Cflg = 0;
	if (argc == 0) {
		argc++;
		argv = &dotp;
	}
	fp = (struct afile *)calloc(argc, sizeof (struct afile));
	if (fp == 0) {
		fprintf(stderr, "ls: out of memory\n");
		exit(1);
	}
	fp0 = fp;
	for (i = 0; i < argc; i++) {
		if (gstat(fp, *argv, 1, (int *)0)) {
			fp->fname = *argv;
			fp->fflags |= ISARG;
			fp++;
		}
		argv++;
	}
	fplast = fp;
	qsort(fp0, fplast - fp0, sizeof (struct afile), fcmp);
	if (dflg) {
		formatf(fp0, fplast);
		exit(0);
	}
	if (fflg)
		fp = fp0;
	else {
		for (fp = fp0; fp < fplast && fp->ftype != 'd'; fp++)
			continue;
		formatf(fp0, fp);
	}
	if (fp < fplast) {
		if (fp > fp0)
			printf("\n");
		for (;;) {
			formatd(fp->fname, argc > 1);
			while (subdirs) {
				struct subdirs *t;

				t = subdirs; subdirs = t->sd_next;
				printf("\n");
				formatd(t->sd_name, 1);
				cfree(t->sd_name);
				cfree((char *)t);
			}
			if (++fp == fplast)
				break;
			printf("\n");
		}
	}
	exit(0);
}

formatd(name, title)
	char *name;
	int title;
{
	register struct afile *fp;
	register struct subdirs *dp;
	struct afile *dfp0, *dfplast;
	int nkb;

	nkb = getdir(name, &dfp0, &dfplast);
	if (dfp0 == 0)
		return;
	if (fflg == 0)
		qsort(dfp0, dfplast - dfp0, sizeof (struct afile), fcmp);
	if (title)
		printf("%s:\n", name);
	if (lflg || sflg)
		printf("total %ld\n", nkb);
	formatf(dfp0, dfplast);
	if (Rflg)
		for (fp = dfplast - 1; fp >= dfp0; fp--) {
			if (fp->ftype != 'd' ||
			    !strcmp(fp->fname, ".") ||
			    !strcmp(fp->fname, ".."))
				continue;
			dp = (struct subdirs *)malloc(sizeof (struct subdirs));
			dp->sd_name = savestr(cat(name, fp->fname));
			dp->sd_next = subdirs; subdirs = dp;
		}
	for (fp = dfp0; fp < dfplast; fp++) {
		if ((fp->fflags&ISARG) == 0 && fp->fname)
			cfree(fp->fname);
		if (fp->flinkto)
			cfree(fp->flinkto);
	}
	cfree((char *)dfp0);
}

getdir(dir, pfp0, pfplast)
	char *dir;
	struct afile **pfp0, **pfplast;
{
	register struct afile *fp;
	DIR *dirp;
	register struct direct *dp;
	int nb, nent = 20;

	dirp = opendir(dir);
	if (dirp == NULL) {
		*pfp0 = *pfplast = NULL;
		printf("%s unreadable\n", dir);		/* not stderr! */
		return (0);
	}
	fp = *pfp0 = (struct afile *)calloc(nent, sizeof (struct afile));
	*pfplast = *pfp0 + nent;
	nb = 0;
	while (dp = readdir(dirp)) {
		if (dp->d_ino == 0)
			continue;
		if (aflg == 0 && dp->d_name[0]=='.' &&
		    (Aflg == 0 || dp->d_name[1]==0 ||
		     dp->d_name[1]=='.' && dp->d_name[2]==0))
			continue;
		if (gstat(fp, cat(dir, dp->d_name), Fflg+Rflg, &nb) == 0)
			continue;
		fp->fnum = dp->d_ino;
		fp->fname = savestr(dp->d_name);
		fp++;
		if (fp == *pfplast) {
			*pfp0 = (struct afile *)realloc((char *)*pfp0,
			    2 * nent * sizeof (struct afile));
			if (*pfp0 == 0) {
				fprintf(stderr, "ls: out of memory\n");
				exit(1);
			}
			fp = *pfp0 + nent;
			*pfplast = fp + nent;
			nent *= 2;
		}
	}
	closedir(dirp);
	*pfplast = fp;
	return (kbytes(dbtob(nb)));
}

int	stat(), lstat();

struct afile *
gstat(fp, file, statarg, pnb)
	register struct afile *fp;
	char *file;
	int statarg, *pnb;
{
	int (*statf)() = Lflg ? stat : lstat;
	char buf[BUFSIZ]; int cc;
	static struct afile azerofile;

	*fp = azerofile;
	fp->fflags = 0;
	fp->fnum = 0;
	fp->ftype = '-';
	if (statarg || sflg || lflg || tflg) {
		struct stat stb, stb1;

		if ((*statf)(file, &stb) < 0) {
			if (statf == lstat || lstat(file, &stb) < 0) {
				fprintf(stderr, "%s not found\n", file);
				return (0);
			}
		}
		fp->fblks = stb.st_blocks;
		fp->fsize = stb.st_size;
		switch (stb.st_mode & S_IFMT) {

		case S_IFDIR:
			fp->ftype = 'd'; break;
		case S_IFBLK:
			fp->ftype = 'b'; fp->fsize = stb.st_rdev; break;
		case S_IFCHR:
			fp->ftype = 'c'; fp->fsize = stb.st_rdev; break;
		case S_IFSOCK:
			fp->ftype = 's'; fp->fsize = 0; break;
		case S_IFLNK:
			fp->ftype = 'l';
			if (lflg) {
				cc = readlink(file, buf, BUFSIZ);
				if (cc >= 0) {
					buf[cc] = 0;
					fp->flinkto = savestr(buf);
				}
				break;
			}
			if (stat(file, &stb1) < 0)
				break;
			if ((stb1.st_mode & S_IFMT) == S_IFDIR) {
				stb = stb1;
				fp->ftype = 'd';
				fp->fsize = stb.st_size;
				fp->fblks = stb.st_blocks;
			}
			break;
		}
		fp->fnum = stb.st_ino;
		fp->fflags = stb.st_mode & ~S_IFMT;
		fp->fnl = stb.st_nlink;
		fp->fuid = stb.st_uid;
		fp->fgid = stb.st_gid;
		if (uflg)
			fp->fmtime = stb.st_atime;
		else if (cflg)
			fp->fmtime = stb.st_ctime;
		else
			fp->fmtime = stb.st_mtime;
		if (pnb)
			*pnb += stb.st_blocks;
	}
	return (fp);
}

formatf(fp0, fplast)
	struct afile *fp0, *fplast;
{
	register struct afile *fp;
	int width = 0, w, nentry = fplast - fp0;
	int i, j, columns, lines;
	char *cp;

	if (fp0 == fplast)
		return;
	if (lflg || Cflg == 0)
		columns = 1;
	else {
		for (fp = fp0; fp < fplast; fp++) {
			int len = strlen(fmtentry(fp));

			if (len > width)
				width = len;
		}
		if (usetabs)
			width = (width + 8) &~ 7;
		else
			width += 2;
		columns = twidth / width;
		if (columns == 0)
			columns = 1;
	}
	lines = (nentry + columns - 1) / columns;
	for (i = 0; i < lines; i++) {
		for (j = 0; j < columns; j++) {
			fp = fp0 + j * lines + i;
			cp = fmtentry(fp);
			printf("%s", cp);
			if (fp + lines >= fplast) {
				printf("\n");
				break;
			}
			w = strlen(cp);
			while (w < width)
				if (usetabs) {
					w = (w + 8) &~ 7;
					putchar('\t');
				} else {
					w++;
					putchar(' ');
				}
		}
	}
}

fcmp(f1, f2)
	register struct afile *f1, *f2;
{

	if (dflg == 0 && fflg == 0) {
		if ((f1->fflags&ISARG) && f1->ftype == 'd') {
			if ((f2->fflags&ISARG) == 0 || f2->ftype != 'd')
				return (1);
		} else {
			if ((f2->fflags&ISARG) && f2->ftype == 'd')
				return (-1);
		}
	}
	if (tflg) {
		if (f2->fmtime == f1->fmtime)
			return (0);
		if (f2->fmtime > f1->fmtime)
			return (rflg);
		return (-rflg);
	}
	if (Sflg) {
		if (f2->fsize == f1->fsize)
			return (0);
		if (f2->fsize > f1->fsize)
			return (rflg);
		return (-rflg);
	}
	return (rflg * strcmp(f1->fname, f2->fname));
}

char *
cat(dir, file)
	char *dir, *file;
{
	static char dfile[BUFSIZ];

	if (strlen(dir)+1+strlen(file)+1 > BUFSIZ) {
		fprintf(stderr, "ls: filename too long\n");
		exit(1);
	}
	if (!strcmp(dir, "") || !strcmp(dir, ".")) {
		(void) strcpy(dfile, file);
		return (dfile);
	}
	(void) strcpy(dfile, dir);
	if (dir[strlen(dir) - 1] != '/' && *file != '/')
		(void) strcat(dfile, "/");
	(void) strcat(dfile, file);
	return (dfile);
}

char *
savestr(str)
	char *str;
{
	char *cp = malloc(strlen(str) + 1);

	if (cp == NULL) {
		fprintf(stderr, "ls: out of memory\n");
		exit(1);
	}
	(void) strcpy(cp, str);
	return (cp);
}

char	*fmtinum(), *fmtsize(), *fmtlstuff(), *fmtmode();

char *
fmtentry(fp)
	register struct afile *fp;
{
	static char fmtres[BUFSIZ];
	register char *cp, *dp;

	(void) sprintf(fmtres, "%s%s%s",
	    iflg ? fmtinum(fp) : "",
	    sflg ? fmtsize(fp) : "",
	    lflg ? fmtlstuff(fp) : "");
	dp = &fmtres[strlen(fmtres)];
	for (cp = fp->fname; *cp; cp++)
		if (qflg && (*cp < ' ' || *cp >= 0177))
			*dp++ = '?';
		else
			*dp++ = *cp;
	if (Fflg) {
		if (fp->ftype == 'd')
			*dp++ = '/';
		else if (fp->ftype == 'l')
			*dp++ = '@';
		else if (fp->ftype == 's')
			*dp++ = '=';
		else if (fp->fflags & 0111)
			*dp++ = '*';
	}
	if (lflg && fp->flinkto) {
		(void) strcpy(dp, " -> "); dp += 4;
		for (cp = fp->flinkto; *cp; cp++)
			if (qflg && (*cp < ' ' || *cp >= 0177))
				*dp++ = '?';
			else
				*dp++ = *cp;
	}
	*dp++ = 0;
	return (fmtres);
}

char *
fmtinum(p)
	register struct afile *p;
{
	static char inumbuf[8];

	(void) sprintf(inumbuf, "%6d ", p->fnum);
	return (inumbuf);
}

char *
fmtsize(p)
	register struct afile *p;
{
	static char sizebuf[32];

	(void) sprintf(sizebuf, "%4ld ", kbytes(dbtob(p->fblks)));
	return (sizebuf);
}

char *
fmtlstuff(p)
	register struct afile *p;
{
	static char lstuffbuf[256];
	char gname[32], uname[32], fsize[32], ftime[32];
	register char *lp = lstuffbuf;

	/* type mode uname gname fsize ftime */
/* get uname */
	{ char *cp = getname(p->fuid);
	  if (cp)
		(void) sprintf(uname, "%-9.9s", cp);
	  else
		(void) sprintf(uname, "%-9d", p->fuid);
	}
/* get gname */
	if (gflg) {
	  char *cp = getgroup(p->fgid);
	  if (cp)
		(void) sprintf(gname, "%-9.9s", cp);
	  else
		(void) sprintf(gname, "%-9d", p->fgid);
	}
/* get fsize */
	if (p->ftype == 'b' || p->ftype == 'c')
		(void) sprintf(fsize, "%3d,%4d",
		    major(p->fsize), minor(p->fsize));
	else if (p->ftype == 's')
		(void) sprintf(fsize, "%8ld", 0);
	else
		(void) sprintf(fsize, "%8ld", p->fsize);
/* get ftime */
	{ char *cp = ctime(&p->fmtime);
	  if ((p->fmtime < sixmonthsago) || (p->fmtime > now))
		(void) sprintf(ftime, " %-7.7s %-4.4s ", cp+4, cp+20);
	  else
		(void) sprintf(ftime, " %-12.12s ", cp+4);
	}
/* splat */
	*lp++ = p->ftype;
	lp = fmtmode(lp, p->fflags);
	(void) sprintf(lp, "%3d %s%s%s%s",
	    p->fnl, uname, gflg ? gname : "", fsize, ftime);
	return (lstuffbuf);
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

char *
fmtmode(lp, flags)
	char *lp;
	int flags;
{
	int **mp;

	for (mp = &m[0]; mp < &m[sizeof(m)/sizeof(m[0])]; ) {
		register int *pairp = *mp++;
		register int n = *pairp++;

		while (--n >= 0 && (flags&*pairp++) == 0)
			pairp++;
		*lp++ = *pairp;
	}
	return (lp);
}

/* rest should be done with nameserver or database */

#include <pwd.h>
#include <grp.h>
#include <utmp.h>

struct	utmp utmp;
#define	NMAX	(sizeof (utmp.ut_name))
#define SCPYN(a, b)	strncpy(a, b, NMAX)

#define NUID	64	/* power of 2 */
#define UIDMASK	0x3f
#define NGID	300

struct ncache {
	int	uid;
	char	name[NMAX+1];
} nc[NUID];
char	outrangename[NMAX+1];
int	outrangeuid = -1;
char	groups[NGID][NMAX+1];
char	outrangegroup[NMAX+1];
int	outrangegid = -1;

char *
getname(uid)
{
	register struct passwd *pw;
	struct passwd *getpwent();
	extern int _pw_stayopen;
	register int cp;

	_pw_stayopen = 1;
	cp = uid & UIDMASK;
	if (uid >= 0 && nc[cp].uid == uid && nc[cp].name[0])
		return (nc[cp].name);
	pw = getpwuid(uid);
	if (!pw)
		return (0);
	nc[cp].uid = uid;
	SCPYN(nc[cp].name, pw->pw_name);
	return (nc[cp].name);
}

char *
getgroup(gid)
{
	register struct group *gr;
	static init;
	struct group *getgrent();

	if (gid >= 0 && gid < NGID && groups[gid][0])
		return (&groups[gid][0]);
	if (gid >= 0 && gid == outrangegid)
		return (outrangegroup);
rescan:
	if (init == 2) {
		if (gid < NGID)
			return (0);
		setgrent();
		while (gr = getgrent()) {
			if (gr->gr_gid != gid)
				continue;
			outrangegid = gr->gr_gid;
			SCPYN(outrangegroup, gr->gr_name);
			endgrent();
			return (outrangegroup);
		}
		endgrent();
		return (0);
	}
	if (init == 0)
		setgrent(), init = 1;
	while (gr = getgrent()) {
		if (gr->gr_gid < 0 || gr->gr_gid >= NGID) {
			if (gr->gr_gid == gid) {
				outrangegid = gr->gr_gid;
				SCPYN(outrangegroup, gr->gr_name);
				return (outrangegroup);
			}
			continue;
		}
		if (groups[gr->gr_gid][0])
			continue;
		SCPYN(groups[gr->gr_gid], gr->gr_name);
		if (gr->gr_gid == gid)
			return (&groups[gid][0]);
	}
	init = 2;
	goto rescan;
}
