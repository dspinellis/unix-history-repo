/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
%sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)print.c	5.24 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <stdio.h>
#include <grp.h>
#include <pwd.h>
#include <utmp.h>
#include <tzfile.h>
#include "ls.h"

printscol(stats, num)
	register LS *stats;
	register int num;
{
	for (; num--; ++stats) {
		(void)printaname(stats);
		(void)putchar('\n');
	}
}

printlong(stats, num)
	LS *stats;
	register int num;
{
	extern int errno;
	char modep[15], *user_from_uid(), *group_from_gid(), *strerror();

	if (f_total)
		(void)printf("total %lu\n", f_kblocks ?
		    howmany(stats[0].lstat.st_btotal, 2) :
		    stats[0].lstat.st_btotal);
	for (; num--; ++stats) {
		if (f_inode)
			(void)printf("%6lu ", stats->lstat.st_ino);
		if (f_size)
			(void)printf("%4ld ", f_kblocks ?
			    howmany(stats->lstat.st_blocks, 2) :
			    stats->lstat.st_blocks);
		(void)strmode(stats->lstat.st_mode, modep);
		(void)printf("%s %3u %-*s ", modep, stats->lstat.st_nlink,
		    UT_NAMESIZE, user_from_uid(stats->lstat.st_uid, 0));
		if (f_group)
			(void)printf("%-*s ", UT_NAMESIZE,
			    group_from_gid(stats->lstat.st_gid, 0));
		if (S_ISCHR(stats->lstat.st_mode) ||
		    S_ISBLK(stats->lstat.st_mode))
			(void)printf("%3d, %3d ", major(stats->lstat.st_rdev),
			    minor(stats->lstat.st_rdev));
		else
			(void)printf("%8ld ", stats->lstat.st_size);
		if (f_accesstime)
			printtime(stats->lstat.st_atime);
		else if (f_statustime)
			printtime(stats->lstat.st_ctime);
		else
			printtime(stats->lstat.st_mtime);
		(void)printf("%s", stats->name);
		if (f_type)
			(void)printtype(stats->lstat.st_mode);
		if (S_ISLNK(stats->lstat.st_mode))
			printlink(stats->name);
		(void)putchar('\n');
	}
}

#define	TAB	8

printcol(stats, num)
	LS *stats;
	int num;
{
	extern int termwidth;
	register int base, chcnt, cnt, col, colwidth;
	int endcol, numcols, numrows, row;

	colwidth = stats[0].lstat.st_maxlen;
	if (f_inode)
		colwidth += 6;
	if (f_size)
		colwidth += 5;
	if (f_type)
		colwidth += 1;

	colwidth = (colwidth + TAB) & ~(TAB - 1);
	if (termwidth < 2 * colwidth) {
		printscol(stats, num);
		return;
	}

	numcols = termwidth / colwidth;
	numrows = num / numcols;
	if (num % numcols)
		++numrows;

	if (f_size && f_total)
		(void)printf("total %lu\n", f_kblocks ?
		    howmany(stats[0].lstat.st_btotal, 2) :
		    stats[0].lstat.st_btotal);
	for (row = 0; row < numrows; ++row) {
		endcol = colwidth;
		for (base = row, chcnt = col = 0; col < numcols; ++col) {
			chcnt += printaname(stats + base);
			if ((base += numrows) >= num)
				break;
			while ((cnt = (chcnt + TAB & ~(TAB - 1))) <= endcol) {
				(void)putchar('\t');
				chcnt = cnt;
			}
			endcol += colwidth;
		}
		putchar('\n');
	}
}

/*
 * print [inode] [size] name
 * return # of characters printed, no trailing characters
 */
printaname(lp)
	LS *lp;
{
	int chcnt;

	chcnt = 0;
	if (f_inode)
		chcnt += printf("%5lu ", lp->lstat.st_ino);
	if (f_size)
		chcnt += printf("%4ld ", f_kblocks ?
		    howmany(lp->lstat.st_blocks, 2) : lp->lstat.st_blocks);
	chcnt += printf("%s", lp->name);
	if (f_type)
		chcnt += printtype(lp->lstat.st_mode);
	return(chcnt);
}

printtime(ftime)
	time_t ftime;
{
	int i;
	char *longstring, *ctime();
	time_t time();

	longstring = ctime((long *)&ftime);
	for (i = 4; i < 11; ++i)
		(void)putchar(longstring[i]);

#define	SIXMONTHS	((DAYSPERNYEAR / 2) * SECSPERDAY)
	if (f_sectime)
		for (i = 11; i < 24; i++)
			(void)putchar(longstring[i]);
	else if (ftime + SIXMONTHS > time((time_t *)NULL))
		for (i = 11; i < 16; ++i)
			(void)putchar(longstring[i]);
	else {
		(void)putchar(' ');
		for (i = 20; i < 24; ++i)
			(void)putchar(longstring[i]);
	}
	(void)putchar(' ');
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
	int lnklen;
	char path[MAXPATHLEN + 1], *strerror();

	if ((lnklen = readlink(name, path, MAXPATHLEN)) == -1) {
		(void)fprintf(stderr, "\nls: %s: %s\n", name, strerror(errno));
		return;
	}
	path[lnklen] = '\0';
	(void)printf(" -> %s", path);
}
