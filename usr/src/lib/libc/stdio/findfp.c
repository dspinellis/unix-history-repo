/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)findfp.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>

#define active(iop)	((iop)->_flag & (_IOREAD|_IOWRT|_IORW))

#define NSTATIC	3	/* stdin + stdout + stderr */

FILE _iob[NSTATIC] = {
	{ 0, NULL, NULL, 0, _IOREAD,		0 },	/* stdin  */
	{ 0, NULL, NULL, 0, _IOWRT,		1 },	/* stdout */
	{ 0, NULL, NULL, 0, _IOWRT|_IONBF,	2 },	/* stderr */
};

static	FILE	*_lastbuf = _iob + NSTATIC;

extern	char	*calloc();

static	FILE	**iobglue;
static	FILE	**endglue;
static	int	nfiles;

FILE *
_findiop()
{
	register FILE **iov;
	register FILE *fp;

	if (nfiles <= 0)
		nfiles = getdtablesize();

	if (iobglue == NULL) {
		iobglue = (FILE **)calloc(nfiles, sizeof *iobglue);
		if (iobglue == NULL)
			return (NULL);

		endglue = iobglue + nfiles;

		iov = iobglue;
		for (fp = _iob; fp < _lastbuf; /* void */)
			*iov++ = fp++;
	}

	iov = iobglue;
	while (*iov != NULL && active(*iov))
		if (++iov >= endglue)
			return (NULL);

	if (*iov == NULL)
		*iov = (FILE *)calloc(1, sizeof **iov);

	return (*iov);
}

_fwalk(function)
	register int (*function)();
{
	register FILE **iov;
	register FILE *fp;

	if (function == NULL)
		return;

	if (iobglue == NULL) {
		for (fp = _iob; fp < _lastbuf; fp++)
			if (active(fp))
				(*function)(fp);
	} else {
		for (iov = iobglue; iov < endglue; iov++)
			if (*iov != NULL && active(*iov))
				(*function)(*iov);
	}
}

_cleanup()
{
	extern int fclose();

	_fwalk(fclose);
}
