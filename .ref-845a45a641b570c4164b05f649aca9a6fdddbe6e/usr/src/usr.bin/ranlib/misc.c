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
static char sccsid[] = "@(#)misc.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/signal.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pathnames.h"

extern char *archive;			/* archive name */
char *tname = "temporary file";		/* temporary file "name" */

tmp()
{
	sigset_t set, oset;
	int fd;
	char *envtmp, path[MAXPATHLEN];

	if ((envtmp = getenv("TMPDIR")) != NULL)
		(void)sprintf(path, "%s%s", envtmp, strrchr(_PATH_RANTMP, '/'));
	else
		bcopy(_PATH_RANTMP, path, sizeof(_PATH_RANTMP));
	
	sigfillset(&set);
	(void)sigprocmask(SIG_BLOCK, &set, &oset);
	if ((fd = mkstemp(path)) == -1)
		error(path);
        (void)unlink(path);
	(void)sigprocmask(SIG_SETMASK, &oset, NULL);
	return(fd);
}

void *
emalloc(len)
	int len;
{
	void *p;

	if ((p = malloc((u_int)len)) == NULL)
		error(archive);
	return(p);
}

char *
rname(path)
	char *path;
{
	register char *ind;

	return((ind = rindex(path, '/')) ? ind + 1 : path);
}

badfmt()
{
	errno = EFTYPE;
	error(archive);
}

error(name)
	char *name;
{
	(void)fprintf(stderr, "ranlib: %s: %s\n", name, strerror(errno));
	exit(1);
}
