/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getgrent.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <stdio.h>
#include <grp.h>

static FILE *_gr_fp;
static struct group _gr_group;
static int _gr_stayopen;
static char *_gr_file = _PATH_GROUP;

#define	MAXGRP		200
static char *members[MAXGRP];
#define	MAXLINELENGTH	1024
static char line[MAXLINELENGTH];

struct group *
getgrent()
{
	if (!_gr_fp && !start_gr() || !grscan(0, 0, (char *)NULL))
		return((struct group *)NULL);
	return(&_gr_group);
}

struct group *
getgrnam(name)
	char *name;
{
	int rval;

	if (!start_gr())
		return((struct group *)NULL);
	rval = grscan(1, 0, name);
	if (!_gr_stayopen)
		endgrent();
	return(rval ? &_gr_group : (struct group *)NULL);
}

struct group *
getgrgid(gid)
	int gid;
{
	int rval;

	if (!start_gr())
		return((struct group *)NULL);
	rval = grscan(1, gid, (char *)NULL);
	if (!_gr_stayopen)
		endgrent();
	return(rval ? &_gr_group : (struct group *)NULL);
}

static
start_gr()
{
	if (_gr_fp) {
		rewind(_gr_fp);
		return(1);
	}
	return((_gr_fp = fopen(_gr_file, "r")) ? 1 : 0);
}

setgrent()
{
	return(setgroupent(0));
}

setgroupent(stayopen)
	int stayopen;
{
	if (!start_gr())
		return(0);
	_gr_stayopen = stayopen;
	return(1);
}

void
endgrent()
{
	if (_gr_fp) {
		(void)fclose(_gr_fp);
		_gr_fp = (FILE *)NULL;
	}
}

void
setgrfile(file)
	char *file;
{
	_gr_file = file;
}

static
grscan(search, gid, name)
	register int search, gid;
	register char *name;
{
	register char *cp, **m;
	char *bp;
	char *fgets(), *strsep(), *index();

	for (;;) {
		if (!fgets(line, sizeof(line), _gr_fp))
			return(0);
		bp = line;
		/* skip lines that are too big */
		if (!index(line, '\n')) {
			int ch;

			while ((ch = getc(_gr_fp)) != '\n' && ch != EOF)
				;
			continue;
		}
		_gr_group.gr_name = strsep(&bp, ":\n");
		if (search && name && strcmp(_gr_group.gr_name, name))
			continue;
		_gr_group.gr_passwd = strsep(&bp, ":\n");
		if (!(cp = strsep(&bp, ":\n")))
			continue;
		_gr_group.gr_gid = atoi(cp);
		if (search && name == NULL && _gr_group.gr_gid != gid)
			continue;
		for (m = _gr_group.gr_mem = members;; ++m) {
			if (m == &members[MAXGRP - 1]) {
				*m = NULL;
				break;
			}
			if ((*m = strsep(&bp, ", \n")) == NULL)
				break;
		}
		return(1);
	}
	/* NOTREACHED */
}
