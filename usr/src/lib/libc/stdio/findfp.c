/* @(#)findfp.c	1.2 (Berkeley) %G% */
#include <stdio.h>

#define NSTATIC	10	/* stdin, stdout, stderr, plus slack */

extern char *calloc();

static FILE *dummy[NSTATIC];
static FILE **iov = NULL;
static FILE **iovend;

FILE _iob[NSTATIC] = {
	{ 0, NULL, NULL, NULL, _IOREAD,		0 },	/* stdin  */
	{ 0, NULL, NULL, NULL, _IOWRT,		1 },	/* stdout */
	{ 0, NULL, NULL, NULL, _IOWRT|_IONBF,	2 },	/* stderr */
};

static char smallbuf[NSTATIC];
static char *unbufp = NULL;

FILE *
_findiop()
{
	register FILE **iovp;
	register FILE *fp;
	register int nfiles;

	if (iov == NULL) {
		unbufp	= NULL;
		iov	= NULL;
		fp	= NULL;

		nfiles = getdtablesize();
		if (nfiles > NSTATIC) {
			fp = (FILE *)calloc(nfiles - NSTATIC, sizeof *fp);
			if (fp != NULL) {
				iov = (FILE **)calloc(nfiles, sizeof *iov);
				if (iov != NULL)
					unbufp = calloc(nfiles, sizeof *unbufp);
			}
		}

		if (unbufp != NULL) {
			iovend = iov + nfiles;
			for (iovp = iov + NSTATIC; iovp < iovend; /* void */)
				*iovp++ = fp++;
		} else {
			if (fp != NULL) {
				free((char *)fp);
				if (iov != NULL)
					free((char *)iov);
			}

			iovend	= dummy + NSTATIC;
			iov	= dummy;
		}

		iovp = iov;
		for (fp = _iob; fp < _iob + NSTATIC; /* void */)
			*iovp++ = fp++;
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

char *
_smallbuf(iop)
	register FILE *iop;
{
	if (unbufp == NULL)
		return (&smallbuf[iop - _iob]);
	else
		return (&unbufp[fileno(iop)]);
}
