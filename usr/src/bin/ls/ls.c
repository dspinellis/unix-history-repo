/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ls.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <dirent.h>
#include <grp.h>
#include <pwd.h>
#include <strings.h>
#include <tzfile.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>

typedef struct _lsstruct {
	struct stat lstat;
	char *name;		/* pointer to filename */
} LS;

int	qflg, Aflg, Cflg, Fflg, Lflg, Rflg, Sflg;

/* flags */
int f_listall;			/* list .xxx files? */
int f_listalways;		/* list . and .. */
int f_modtime;			/* use time of last change for time */
int f_accesstime;		/* use time of last access */
int f_statustime;		/* use time of last mode change */
int f_singlecol;		/* use single column output */
int f_listdir;			/* list actual directory, not contents */
int f_specialdir;		/* force params to be directories */
int f_type;			/* add /, * @ = to mark non-regular files */
int f_group;			/* show group ownership of a file */
int f_inode;			/* print inode */
int f_longform;			/* long listing format */
int f_nonprint;			/* show unprintables as ? */
int f_reversesort;		/* reverse whatever sort is used */
int f_recursive;		/* ls subdirectories also */
int f_size;			/* list size in short listing */
int f_timesort;			/* sort by time vice name */
int f_number;			/* list UID and GID by number */
int f_firsttime = 1;		/* to control recursion */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind, stat();
	int ch;
	int namecmp(), revnamecmp(), acccmp(), revacccmp();
	int modcmp(), revmodcmp(), statcmp(), revstatcmp();

	 /* set up defaults for terminal/nonterminal stdout */
	if (isatty(1)) {
		f_nonprint = 1;
		lengthfcn = prablelen;
	} else
		f_singlecol = 1;

	/* root sees all files automatically */
	if (!getuid())
		f_listall = 1;

	while ((ch = getopt(argc, argv, "1ACFLRacdfgilqrstu")) != EOF) {
		switch (ch) {
		case '1':
			f_singlecol = 1;
			break;
		case 'C':
			f_singlecol = 0;
			break;
		case 'F':
			f_type = 1;
			break;
		case 'L':
			statfcn = stat;
			break;
		case 'R':
			f_recursive = 1;
			statfcn = lstat;
			break;
		case 'a':
			f_listalways = 1;
			/* FALLTHROUGH */
		case 'A':
			f_listall = 1;
			break;
		case 'c':
			f_statustime = 1;
			f_modtime = f_accesstime = 0;
			break;
		case 'S':
			Sflg++; /* fall into... */
		case 'd':
			f_listdir = 1;
			break;
		case 'f':
			f_specialdir = 1;
			break;
		case 'g':
			f_group = 1;
			break;
		case 'i':
			f_inode = 1;
			break;
		case 'l':
			f_longform = 1;
			f_number = f_singlecol = 0;
			statfcn = lstat;
			if (!f_accesstime && !f_statustime)
				f_modtime = 1;
			break;
		case 'q':
			f_nonprint = 1;
			lengthfcn = prablelen;
			break;
		case 'r':
			f_reversesort = 1;
			break;
		case 's':
			f_size = 1;
			statfcn = lstat;
			break;
		case 't':
			f_timesort = 1;
			if (!f_accesstime && !f_statustime)
				f_modtime = 1;
			break;
		case 'u':
			f_modtime = f_statustime = 0;
			f_accesstime = 1;
			break;
		default:
		case '?':
			usage();
		}
	}

	/* -f turns off -l, -t, -s, -r, turns on -a */
	if (f_specialdir) {
		f_longform = f_timesort = f_size = f_reversesort = 0;
		f_listall = f_listalways = 1;
	}

	/* select a sort function */
	if (f_reversesort) {
		if (!f_timesort)
			sortfcn = revnamecmp;
		else if (f_accesstime)
			sortfcn = revacccmp;
		else if (f_modtime)
			sortfcn = revmodcmp;
		else if (f_statustime)
			sortfcn = revstatcmp;
	} else {
		if (!f_timesort)
			sortfcn = namecmp;
		else if (f_accesstime)
			sortfcn = acccmp;
		else if (f_modtime)
			sortfcn = modcmp;
		else if (f_statustime)
			sortfcn = statcmp;
	}

	if (argc > optind)
		lsdir(argc - optind, &argv[optind]);
	else {
		char *defname = ".";

		lsdir(1, &defname);
	}
	exit(0);
}

/*
 * list the files in a directory
 */
extern int errno;

lsdir(argc, argv)
	int argc;		/* count of file names passed */
	char **argv;		/* array of file names */
{
	static char curpath[MAXPATHLEN + 1];
	register char *p;
	LS *stats;
	int curname, entries, i, needstat, neednewline;
	char *emalloc();

	stats = (LS *)emalloc((u_int)argc * (sizeof(LS)));

	needstat = f_firsttime || f_longform || f_recursive || f_timesort ||
	    f_size || f_inode || f_type;
	neednewline = !f_firsttime;

	for (entries = curname = 0; curname < argc; ++curname) {
		if (!f_firsttime) {
			/*
			 * check for .xxx files.  Note can't get listalways
			 * without listall being set.
			 */
			p = argv[curname];
			if (p[0] == '.') {
				if (!f_listall)
					continue;
				/* and now for listalways: . and .. */
				if ((!p[1] || p[1] == '.' && !p[2]) &&
				    !f_listalways)
					continue;
			}
		}
		if (needstat) {
			if (statfcn(argv[curname], &stats[entries].lstat)) {
				(void)fprintf(stderr, "ls: %s: %s\n",
				    argv[curname], strerror(errno));
				if (errno == ENOENT)
					continue;
				exit(1);
			}
			if (!S_ISDIR(stats[entries].lstat.st_mode))
				neednewline = 1;
		}
		stats[entries++].name = argv[curname];
	}
	if (!entries)
		return;

	if (entries > 1 && !f_specialdir)
		qsort((char *)stats, entries, sizeof(LS), sortfcn);

	prindir(stats, entries);

	if ((f_firsttime && !f_listdir) || f_recursive) {
		for (i = 0; i < entries; ++i) {
			/* recurse on directories */
			if (S_ISDIR(stats[i].lstat.st_mode)) {
				/* don't recurse on . or .. */
				p = stats[i].name;
				if (p[0] == '.' && (!p[1] ||
				    p[1] == '.' && !p[2]) &&
				    !f_firsttime)
					continue;
				if (chdir(p)) {
					(void)fprintf(stderr, "ls: %s: %s\n",
					    p, strerror(errno));
					break;
				}
				if (entries > 1 ||
				    !f_firsttime || f_recursive) {
					/* add current name to path */
					p = rindex(curpath, '\0');
					if (p != curpath && p[-1] != '/')
						*p++ = '/';
					(void)strcpy(p, stats[i].name);
					if (entries > 1 || !f_firsttime) {
						if (neednewline)
							(void)putchar('\n');
						(void)printf("%s:\n",
						    curpath);
					}
				}
				recursedir(&stats[i]);
				if (entries > 1 || !f_firsttime)
					*p = '\0';
				if (chdir("..")) {
					(void)fprintf(stderr, "ls: ..: %s\n",
					    strerror(errno));
					exit(1);
				}
			}
		}
		f_firsttime = 0;
	}
	free((char *)stats);
}

/*
 * set up the call to lsdir (lsdir(count, argv-type array))
 * mutually recursive with lsdir
 */
recursedir(orig)
	LS *orig;
{
	DIR *dirp;
	struct dirent *entry;
	int blocksub, vsub, oldfirsttimeflag;
	char **argv, *argvblock, *emalloc();

	blocksub = vsub = 0;

	if ((dirp = opendir(".")) == NULL) {
		(void)fprintf(stderr, "ls: %s: %s\n",
		    orig->name, strerror(errno));
		return;
	}
	/* wildly overestimate dynamic amount of memory needed */
	argvblock = emalloc((u_int)orig->lstat.st_size);
	argv = (char **)emalloc((u_int)orig->lstat.st_size);

	while ((entry = readdir(dirp)) != NULL) {
		argv[vsub++] = strncpy(&argvblock[blocksub], entry->d_name,
		    (int)entry->d_namlen);
		blocksub += entry->d_namlen;
		argvblock[blocksub++] = '\0';
	}
	(void)closedir(dirp);

	oldfirsttimeflag = f_firsttime;
	f_firsttime = 0;

	lsdir(vsub, argv);

	f_firsttime = oldfirsttimeflag;

	free(argvblock);
	free((char *)argv);
}

/*
 * print a statted (if necessary), sorted directory by listing option
 */
prindir(stats, endcount)
	LS *stats;		/* the statted, sorted directory */
	register int endcount;
{
	register int entry;
	int i;			/* subscript to stats */
	int maxlen;		/* length of longest name string */
	int colwidth;		/* width of a printing column */
	int numcols;		/* number of columns */
	int collength;		/* lines in longest column */
	int base;		/* subscript for leftmost column */
	int offset;		/* delta from base to next column */
	int chcnt;		/* character count printed */
	long blocks;		/* sum of blocks in longform listing */

	if (f_singlecol) {
		for (entry = 0; entry < endcount; ++entry)
			if (!f_firsttime ||
			    !S_ISDIR(stats[entry].lstat.st_mode)) {
				(void)printaname(&stats[entry]);
				(void)putchar('\n');
			}
		return;
	}

	if (f_longform) {
		if (!f_firsttime) {
			for (i = 0, blocks = 0; i < endcount; ++i)
				blocks += stats[i].lstat.st_blocks;
			(void)printf("total %ld\n", blocks / 2);
		}
		for (i = 0; i < endcount; ++i) {
			if (f_inode)
				(void)printf("%6lu ", stats[i].lstat.st_ino);
			if (f_size)
				(void)printf("%4ld ",
				    stats[i].lstat.st_blocks / 2);
			printperms(stats[i].lstat.st_mode);
			(void)printf("%3d ", stats[i].lstat.st_nlink);
			printowner(stats[i].lstat.st_uid);
			if (f_group)
				printgrp(stats[i].lstat.st_gid);
			if (S_ISCHR(stats[i].lstat.st_mode) ||
			    S_ISBLK(stats[i].lstat.st_mode))
				(void)printf("%3d,%4d ",
				    major(stats[i].lstat.st_rdev),
				    minor(stats[i].lstat.st_rdev));
			else
				(void)printf("%8ld ", stats[i].lstat.st_size);
			if (f_accesstime)
				printtime(stats[i].lstat.st_atime);
			else if (f_statustime)
				printtime(stats[i].lstat.st_ctime);
			else
				printtime(stats[i].lstat.st_mtime);
			(void)printf("%s", stats[i].name);
			if (f_type)
				(void)printtype(stats[i].lstat.st_mode);
			if (S_ISLNK(stats[i].lstat.st_mode))
				printlink(stats[i].name);
			(void)putchar('\n');
		}
		return;
	}

	/*
	 * assume tabs every 8 columns WARNING: bad code (hard coded
	 * constants) follows:
	 */

	/* figure out max width */
	maxlen = -1;
	for (i = 0; i < endcount; ++i) {
		if (maxlen < lengthfcn(stats[i].name))
			maxlen = lengthfcn(stats[i].name);
	}

	/* add fudge factors to max name length */
	if (f_inode)
		maxlen += 6;
	if (f_size)
		maxlen += 5;
	if (f_type)
		maxlen += 1;
	/* one tab after maxlen */
	colwidth = (maxlen + 9) & ~0x7;
	numcols = (80 + colwidth - maxlen) / colwidth;
	collength = (int)((float)endcount / (float)numcols + 0.999);

	for (base = 0; base < collength; base++) {
		for (offset = 0, i = 0; i < numcols; ++i, offset += collength) {
			if (base + offset >= endcount)
				break;
			chcnt = printaname(&stats[base + offset]);
			if (base + offset + collength < endcount) {
				while (chcnt + 8 < colwidth) {
					(void)putchar('\t');
					chcnt += 8;
				}
				if (chcnt < colwidth)
					(void)putchar('\t');
				chcnt = (chcnt + 8) & ~0x7;
			}
		}
		if (base + offset < endcount)
			(void)printaname(&stats[base + offset]);
		(void)printf("\n");
	}
}

/*
 * print [inode] [size] name
 * return # of characters printed, no trailing characters
 */
printaname(entry)
	LS *entry;
{
	int chcnt = 0;

	if (f_inode)
		chcnt += printf("%5lu ", entry->lstat.st_ino);
	if (f_size)
		chcnt += printf("%4ld ", entry->lstat.st_blocks / 2);
	chcnt += printf("%s", entry->name);
	if (f_type)
		chcnt += printtype(entry->lstat.st_mode);
	return(chcnt);
}

/*
 * print group and user name
 */
printgrp(gid)
	gid_t gid;
{
	struct group *groupentry;

	if ((groupentry = getgrgid((int)gid)) == NULL) {
		/* can't find group, print out number instead */
		(void)printf("%-9u ", gid);
		return;
	}
	(void)printf("%-9s", groupentry->gr_name);
	(void)getgrent();		/* to rewind group file */
}

printowner(uid)
	uid_t uid;
{
	struct passwd *pwentry;

	if ((pwentry = getpwuid((int)uid)) == NULL) {
		/* can't find owner, print out number instead */
		(void)printf("%-9u ", uid);
		return;
	}
	(void)printf("%-9s", pwentry->pw_name);
	(void)getpwent();
}

#define	SIXMONTHS	((DAYSPERNYEAR / 2) * SECSPERDAY)
time_t time();

printtime(ftime)
	time_t ftime;
{
	int i;
	char *longstring;

	longstring = ctime((long *)&ftime);
	for (i = 4; i < 11; ++i)
		(void)putchar(longstring[i]);

	if (ftime + SIXMONTHS > time((time_t *)NULL))
		for (i = 11; i < 16; ++i)
			(void)putchar(longstring[i]);
	else {
		(void)putchar(' ');
		for (i = 20; i < 24; ++i)
			(void)putchar(longstring[i]);
	}
	(void)putchar(' ');
}

/*
 * act like strlen, but also translate non-printing chars to '?'
 */
prablelen(cp)
	char *cp;
{
	register int len = 0;

	for (; *cp; ++len, ++cp)
		if (!isprint(*cp))
			*cp = '?';
	return(len);
}

/*
 * do the permissions printing, passed the mode
 */
printperms(mode)
	mode_t mode;
{
	 /* print type */
	switch (mode & S_IFMT) {
	case S_IFDIR:			/* directory */
		(void)putchar('d');
		break;
	case S_IFCHR:			/* character special */
		(void)putchar('c');
		break;
	case S_IFBLK:			/* block special */
		(void)putchar('b');
		break;
	case S_IFREG:			/* regular */
		(void)putchar('-');
		break;
	case S_IFLNK:			/* symbolic link */
		(void)putchar('l');
		break;
	case S_IFSOCK:			/* socket */
		(void)putchar('s');
		break;
#ifdef S_IFIFO
	case S_IFIFO:			/* fifo */
		(void)putchar('p');
		break;
#endif
	default:			/* unknown */
		(void)putchar('?');
		break;
	}
	/* usr */
	if (mode & S_IRUSR)
		(void)putchar('r');
	else
		(void)putchar('-');
	if (mode & S_IWUSR)
		(void)putchar('w');
	else
		(void)putchar('-');
	switch (mode & (S_IXUSR | S_ISUID)) {
	case 0:
		(void)putchar('-');
		break;
	case S_IXUSR:
		(void)putchar('x');
		break;
	case S_ISUID:
		(void)putchar('S');
		break;
	case S_IXUSR | S_ISUID:
		(void)putchar('s');
		break;
	}
	/* group */
	if (mode & S_IRGRP)
		(void)putchar('r');
	else
		(void)putchar('-');
	if (mode & S_IWGRP)
		(void)putchar('w');
	else
		(void)putchar('-');
	switch (mode & (S_IXGRP | S_ISGID)) {
	case 0:
		(void)putchar('-');
		break;
	case S_IXGRP:
		(void)putchar('x');
		break;
	case S_ISGID:
		(void)putchar('S');
		break;
	case S_IXGRP | S_ISGID:
		(void)putchar('s');
		break;
	}
	/* other */
	if (mode & S_IROTH)
		(void)putchar('r');
	else
		(void)putchar('-');
	if (mode & S_IWOTH)
		(void)putchar('w');
	else
		(void)putchar('-');
	switch (mode & (S_IXOTH | S_ISVTX)) {
	case 0:
		(void)putchar('-');
		break;
	case S_IXOTH:
		(void)putchar('x');
		break;
	case S_ISVTX:
		(void)putchar('T');
		break;
	case S_IXOTH | S_ISVTX:
		(void)putchar('t');
		break;
	}
}

printtype(mode)
	mode_t mode;
{
	switch(mode & S_IFMT) {
	case S_IFDIR:
		(void)putchar('/');
		return(1);
	case S_IFLNK:
		(void)putchar('@');
		return(1);
	case S_IFSOCK:
		(void)putchar('=');
		return(1);
	}
	if (mode & (S_IXUSR | S_IXGRP | S_IXOTH)) {
		(void)putchar('*');
		return(1);
	}
	return(0);
}

printlink(name)
	char *name;
{
	char path[MAXPATHLEN + 1];
	int lnklen;

	if ((lnklen = readlink(name, path, MAXPATHLEN)) == -1) {
		(void)fprintf(stderr, "\nls: %s: %s\n", name, strerror(errno));
		return;
	}
	path[lnklen] = '\0';
	(void)printf(" -> %s", path);
}

namecmp(a, b)
	LS *a, *b;
{
	return(strcmp(a->name, b->name));
}

revnamecmp(a, b)
	LS *a, *b;
{
	return(strcmp(b->name, a->name));
}

modcmp(a, b)
	LS *a, *b;
{
	return(a->lstat.st_mtime < b->lstat.st_mtime);
}

revmodcmp(a, b)
	LS *a, *b;
{
	return(b->lstat.st_mtime < a->lstat.st_mtime);
}

acccmp(a, b)
	LS *a, *b;
{
	return(a->lstat.st_atime < b->lstat.st_atime);
}

revacccmp(a, b)
	LS *a, *b;
{
	return(b->lstat.st_atime < a->lstat.st_atime);
}

statcmp(a, b)
	LS *a, *b;
{
	return(a->lstat.st_ctime < b->lstat.st_ctime);
}

revstatcmp(a, b)
	LS *a, *b;
{
	return(b->lstat.st_ctime < a->lstat.st_ctime);
}

char
*emalloc(size)
	u_int size;
{
	char *retval, *malloc();

	if ((retval = malloc(size)) == NULL) {
		(void)fprintf(stderr, "ls: out of memory.\n");
		exit(1);
	}
	return(retval);
}

usage()
{
	(void)fprintf(stderr, "usage: ls [-1ACFLRacdfgilqrstu] [file ...]\n");
	exit(1);
}
