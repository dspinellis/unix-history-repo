/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Hugh Smith at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/errno.h>
#include <signal.h>
#include <dirent.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "archive.h"
#include "extern.h"
#include "pathnames.h"

extern CHDR chdr;			/* converted header */
extern char *archive;			/* archive name */
char *tname = "temporary file";		/* temporary file "name" */

tmp()
{
	extern char *envtmp;
	sigset_t set, oset;
	static int first;
	int fd;
	char path[MAXPATHLEN];

	if (!first && !envtmp) {
		envtmp = getenv("TMPDIR");
		first = 1;
	}

	if (envtmp)
		(void)sprintf(path, "%s/%s", envtmp, _NAME_ARTMP);
	else
		bcopy(_PATH_ARTMP, path, sizeof(_PATH_ARTMP));
	
	sigfillset(&set);
	(void)sigprocmask(SIG_BLOCK, &set, &oset);
	if ((fd = mkstemp(path)) == -1)
		error(tname);
        (void)unlink(path);
	(void)sigprocmask(SIG_SETMASK, &oset, NULL);
	return(fd);
}

/*
 * files --
 *	See if the current file matches any file in the argument list; if it
 * 	does, remove it from the argument list.
 */
char *
files(argv)
	char **argv;
{
	register char **list;
	char *p;

	for (list = argv; *list; ++list)
		if (compare(*list)) {
			p = *list;
			for (; list[0] = list[1]; ++list);
			return(p);
		}
	return(NULL);
}

void
orphans(argv)
	char **argv;
{
	for (; *argv; ++argv)
		(void)fprintf(stderr,
		    "ar: %s: not found in archive.\n", *argv);
}

char *
rname(path)
	char *path;
{
	register char *ind;

	return((ind = rindex(path, '/')) ? ind + 1 : path);
}

compare(dest)
	char *dest;
{
	if (options & AR_TR)
		return(!strncmp(chdr.name, rname(dest), OLDARMAXNAME));
	return(!strcmp(chdr.name, rname(dest)));
}

void
badfmt()
{
	errno = EFTYPE;
	error(archive);
}

void
error(name)
	char *name;
{
	(void)fprintf(stderr, "ar: %s: %s\n", name, strerror(errno));
	exit(1);
}
