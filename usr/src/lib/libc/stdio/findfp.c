/*
 * Copyright (c) 1983, 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)findfp.c	5.3 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <errno.h>

extern int errno;

#define active(iop)	((iop)->_flag & (_IOREAD|_IOWRT|_IORW))

#define NSTATIC	20	/* stdin + stdout + stderr + the usual */

FILE _iob[NSTATIC] = {
	{ 0, NULL, NULL, 0, _IOREAD,		0 },	/* stdin  */
	{ 0, NULL, NULL, 0, _IOWRT,		1 },	/* stdout */
	{ 0, NULL, NULL, 0, _IOWRT|_IONBF,	2 },	/* stderr */
};

extern	char	*calloc();

static	FILE	**iobglue;
static	FILE	**endglue;
static	int	nfiles;

FILE *
_findiop()
{
	register FILE **iov;
	register FILE *fp;

	if (iobglue == 0 && _stdio_init() == 0) {
		errno = ENOMEM;
		return (NULL);
	}

	iov = iobglue;
	while (*iov != NULL && active(*iov))
		if (++iov >= endglue) {
			errno = EMFILE;
			return (NULL);
		}

	if (*iov == NULL)
		*iov = (FILE *)calloc(1, sizeof **iov);

	return (*iov);
}

_stdio_init()
{
	register FILE **iov;
	register FILE *fp;

	nfiles = getdtablesize();

	iobglue = (FILE **)calloc(nfiles, sizeof *iobglue);
	if (iobglue == NULL)
		return (0);

	endglue = iobglue + nfiles;

	for (fp = _iob, iov = iobglue; fp < &_iob[NSTATIC]; /* void */)
		*iov++ = fp++;
	return (1);
}

f_prealloc()
{
	register FILE **iov;
	register FILE *fp;

	if (iobglue == NULL && _stdio_init() == 0)
		return;

	for (iov = iobglue; iov < endglue; iov++)
		if (*iov == NULL)
			*iov = (FILE *)calloc(1, sizeof **iov);
}

_fwalk(function)
	register int (*function)();
{
	register FILE **iov;
	register FILE *fp;

	if (iobglue == NULL) {
		for (fp = _iob; fp < &_iob[NSTATIC]; fp++)
			if (active(fp))
				(*function)(fp);
	} else {
		for (iov = iobglue; iov < endglue; iov++)
			if (*iov && active(*iov))
				(*function)(*iov);
	}
}

_cleanup()
{
	extern int fclose();

	_fwalk(fclose);
}
