/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)print.c	5.28 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <fts.h>
#include <time.h>
#include <errno.h>
#include <grp.h>
#include <pwd.h>
#include <utmp.h>
#include <tzfile.h>
#include <stdio.h>
#include "ls.h"
#include "extern.h"
	
static char	*flags_from_fid __P((long));
static int	 printaname __P((FTSENT *));
static void	 printlink __P((FTSENT *));
static void	 printtime __P((time_t));
static int	 printtype __P((u_int));

#define	IS_NOPRINT(e)	((e)->fts_number == NO_PRINT)

/* ARGUSED */
void
printscol(dlist, entries, btotal, maxlen)
	FTSENT *dlist;
	int entries, maxlen;
	u_long btotal;
{
	register FTSENT *p;

	for (p = dlist; p; p = p->fts_link)
		if (!IS_NOPRINT(p)) {
			(void)printaname(p);
			(void)putchar('\n');
		}
}

/* ARGUSED */
void
printlong(dlist, entries, btotal, maxlen)
	FTSENT *dlist;
	int entries, maxlen;
	u_long btotal;
{
	register FTSENT *p;
	register struct stat *sp;
	char modep[15], *user_from_uid(), *group_from_gid();
	
	
	if (dlist->fts_level != FTS_ROOTLEVEL && (f_longform || f_size))
		(void)printf("total %lu\n",
		    f_kblocks ? howmany(btotal, 2) : btotal);
	for (p = dlist; p; p = p->fts_link) {
		if (IS_NOPRINT(p))
			continue;
		sp = p->fts_statp;
		if (f_inode)
			(void)printf("%6lu ", sp->st_ino);
		if (f_size)
			(void)printf("%4ld ", f_kblocks ?
			    howmany(sp->st_blocks, 2) : sp->st_blocks);
		(void)strmode(sp->st_mode, modep);
		(void)printf("%s %3u %-*s ", modep, sp->st_nlink,
		    UT_NAMESIZE, user_from_uid(sp->st_uid, 0));
		if (f_group)
			(void)printf("%-*s ",
			    UT_NAMESIZE, group_from_gid(sp->st_gid, 0));
		if (f_flags)
			(void)printf("%-*s ",
			    FLAGSWIDTH, flags_from_fid(sp->st_flags));
		if (S_ISCHR(sp->st_mode) ||
		    S_ISBLK(sp->st_mode))
			(void)printf("%3d, %3d ",
			    major(sp->st_rdev), minor(sp->st_rdev));
		else
			(void)printf("%8ld ", sp->st_size);
		if (f_accesstime)
			printtime(sp->st_atime);
		else if (f_statustime)
			printtime(sp->st_ctime);
		else
			printtime(sp->st_mtime);
		(void)printf("%s", p->fts_name);
		if (f_type)
			(void)printtype(sp->st_mode);
		if (S_ISLNK(sp->st_mode))
			printlink(p);
		(void)putchar('\n');
	}
}

#define	TAB	8

void
printcol(dlist, entries, btotal, maxlen)
	FTSENT *dlist;
	u_long btotal;
	int entries, maxlen;
{
	extern int termwidth;
	static FTSENT **array;
	static int lastentries = -1;
	register FTSENT *p;
	register int base, chcnt, cnt, col, colwidth, num;
	int endcol, numcols, numrows, row;

	/*
	 * Have to do random access in the linked list -- build a table
	 * of pointers.
	 */
	if (entries > lastentries) {
		lastentries = entries;
		if ((array = (FTSENT **) realloc(array, entries * sizeof(FTSENT *))) == NULL) {
			err(0, "%s", strerror(errno));
			printscol(dlist, entries, btotal, maxlen);
		}
	}
	for (p = dlist, num = 0; p; p = p->fts_link)
		if (p->fts_number != NO_PRINT)
			array[num++] = p;

	colwidth = maxlen;
	if (f_inode)
		colwidth += 6;
	if (f_size)
		colwidth += 5;
	if (f_type)
		colwidth += 1;

	colwidth = (colwidth + TAB) & ~(TAB - 1);
	if (termwidth < 2 * colwidth) {
		printscol(dlist, 0, 0, 0);
		return;
	}

	numcols = termwidth / colwidth;
	numrows = num / numcols;
	if (num % numcols)
		++numrows;

	if (dlist->fts_level != FTS_ROOTLEVEL && (f_longform || f_size))
		(void)printf("total %lu\n",
		    f_kblocks ? howmany(btotal, 2) : btotal);
	for (row = 0; row < numrows; ++row) {
		endcol = colwidth;
		for (base = row, chcnt = col = 0; col < numcols; ++col) {
			chcnt += printaname(array[base]);
			if ((base += numrows) >= num)
				break;
			while ((cnt = (chcnt + TAB & ~(TAB - 1))) <= endcol) {
				(void)putchar('\t');
				chcnt = cnt;
			}
			endcol += colwidth;
		}
		(void)putchar('\n');
	}
}

/*
 * print [inode] [size] name
 * return # of characters printed, no trailing characters.
 */
static int
printaname(p)
	register FTSENT *p;
{
	struct stat *sp;
	int chcnt;

	sp = p->fts_statp;
	chcnt = 0;
	if (f_inode)
		chcnt += printf("%5lu ", sp->st_ino);
	if (f_size)
		chcnt += printf("%4ld ", f_kblocks ?
		    howmany(sp->st_blocks, 2) : sp->st_blocks);
	chcnt += printf("%s", p->fts_name);
	if (f_type)
		chcnt += printtype(sp->st_mode);
	return (chcnt);
}

static void
printtime(ftime)
	time_t ftime;
{
	int i;
	char *longstring;

	longstring = ctime((time_t *)&ftime);
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

static int
printtype(mode)
	u_int mode;
{
	switch(mode & S_IFMT) {
	case S_IFDIR:
		(void)putchar('/');
		return (1);
	case S_IFLNK:
		(void)putchar('@');
		return (1);
	case S_IFSOCK:
		(void)putchar('=');
		return (1);
	}
	if (mode & (S_IXUSR | S_IXGRP | S_IXOTH)) {
		(void)putchar('*');
		return (1);
	}
	return (0);
}

static void
printlink(p)
	FTSENT *p;
{
	int lnklen;
	char name[MAXPATHLEN + 1], path[MAXPATHLEN + 1];
	
	(void)snprintf(name,
	    MAXPATHLEN, "%s/%s", p->fts_parent->fts_name,  p->fts_name);
	if ((lnklen = readlink(name, path, MAXPATHLEN)) == -1) {
		(void)fprintf(stderr, "\nls: %s: %s\n", name, strerror(errno));
		return;
	}
	path[lnklen] = '\0';
	(void)printf(" -> %s", path);
}

static char *
flags_from_fid(flags)
	long flags;
{
	static char buf[FLAGSWIDTH + 1];
	int size;

	size = FLAGSWIDTH;
	buf[0] = '\0';
	if (size && (flags & NODUMP)) {
		strncat(buf, "nodump", size);
		size -= 6;
	} else {
		strncat(buf, "dump", size);
		size -= 4;
	}
	if (size && (flags & IMMUTABLE)) {
		strncat(buf, ",nochg", size);
		size -= 6;
	}
	if (size && (flags & ARCHIVED)) {
		strncat(buf, ",arch", size);
		size -= 5;
	}
	return (buf);
}
