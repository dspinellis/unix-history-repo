/* @(#)findfp.c	1.1 (Berkeley) %G% */
#include "stdio.h"

#define NSTATIC	5	/* stdin, stdout, stderr, plus slack */

extern char *calloc();

static FILE **iov, **iovend;
static FILE *dummy[NSTATIC];

FILE _iob[NSTATIC] = {
	{ 0, NULL, NULL, NULL, _IOREAD, 0 },		/* stdin  */
	{ 0, NULL, NULL, NULL, _IOWRT, 1 },		/* stdout */
	{ 0, NULL, NULL, NULL, _IOWRT+_IONBF, 2 },	/* stderr */
};

FILE *
_findiop()
{
	register FILE **iovp;
	register FILE *fp;
	register int nfiles;
	register int i;
	char *p;

	if (iov == NULL) {
		nfiles = getdtablesize();
		if (nfiles > NSTATIC)
			p = calloc(1, nfiles * sizeof *iov +
				    (nfiles - NSTATIC) * sizeof **iov);
		else
			p = NULL;

		if (p == NULL) {
			iov = dummy;
			iovend = iov + NSTATIC;
		} else {
			iov = (FILE **)p;
			iovend = iov + nfiles;

			fp = (FILE *)iovend;
			for (iovp = iov + NSTATIC; iovp < iovend; /* void */)
				*iovp++ = fp++;
		}

		for (i = 0; i < NSTATIC; i++)
			iov[i] = &_iob[i];
	}

	for (iovp = iov; (*iovp)->_flag & (_IOREAD|_IOWRT|_IORW); /* void */)
		if (++iovp >= iovend)
			return (NULL);

	return (*iovp);
}

_cleanup()
{
	register FILE *_lastbuf = _iob + NSTATIC;
	register FILE **iovp;
	register FILE *iop;

	if (iov == NULL)
		for (iop = _iob; iop < _lastbuf; iop++)
			fclose(iop);
	else
		for (iovp = iov; iovp < iovend; iovp++)
			fclose(*iovp);
}
