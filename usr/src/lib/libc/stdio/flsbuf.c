/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)flsbuf.c	5.5 (Berkeley) 6/18/90";
#endif LIBC_SCCS and not lint

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>

char	*malloc();

_flsbuf(c, iop)
unsigned char c;
register FILE *iop;
{
	register char *base;
	register n, rn;
	char c1;
	int size;
	struct stat stbuf;

	if (iop->_flag & _IORW) {
		iop->_flag |= _IOWRT;
		iop->_flag &= ~(_IOEOF|_IOREAD);
	}

	if ((iop->_flag&_IOWRT)==0)
		return(EOF);
tryagain:
	if (iop->_flag&_IOLBF) {
		base = iop->_base;
		*iop->_ptr++ = c;
		if ((rn = iop->_ptr - base) >= iop->_bufsiz || c == '\n') {
			iop->_ptr = base;
			iop->_cnt = 0;
		} else {
			/* we got here because _cnt is wrong, so fix it */
			iop->_cnt = -rn;
			rn = n = 0;
		}
	} else if (iop->_flag&_IONBF) {
		c1 = c;
		rn = 1;
		base = &c1;
		iop->_cnt = 0;
	} else {
		if ((base=iop->_base)==NULL) {
			if (fstat(fileno(iop), &stbuf) < 0 ||
			    stbuf.st_blksize <= NULL)
				size = BUFSIZ;
			else
				size = stbuf.st_blksize;
			if ((iop->_base=base=malloc(size)) == NULL) {
				iop->_flag |= _IONBF;
				goto tryagain;
			}
			iop->_flag |= _IOMYBUF;
			iop->_bufsiz = size;
			if (iop==stdout && isatty(fileno(stdout))) {
				iop->_flag |= _IOLBF;
				iop->_ptr = base;
				goto tryagain;
			}
			rn = n = 0;
		} else
			rn = iop->_ptr - base;
		iop->_ptr = base;
		iop->_cnt = iop->_bufsiz;
	}
	while (rn > 0) {
		if ((n = write(fileno(iop), base, rn)) <= 0) {
			iop->_flag |= _IOERR;
			return(EOF);
		}
		rn -= n;
		base += n;
	}
	if ((iop->_flag&(_IOLBF|_IONBF)) == 0) {
		iop->_cnt--;
		*iop->_ptr++ = c;
	}
	return(c);
}

fpurge(iop)
register FILE *iop;
{
	iop->_ptr = iop->_base;
	iop->_cnt = iop->_flag&(_IOLBF|_IONBF|_IOREAD) ? 0 : iop->_bufsiz;
	return(0);
}

fflush(iop)
register FILE *iop;
{
	register char *base;
	register n, rn;

	if ((iop->_flag&(_IONBF|_IOWRT))==_IOWRT &&
	    (base = iop->_base) != NULL && (rn = n = iop->_ptr - base) > 0) {
		iop->_ptr = base;
		iop->_cnt = (iop->_flag&(_IOLBF|_IONBF)) ? 0 : iop->_bufsiz;
		do {
			if ((n = write(fileno(iop), base, rn)) <= 0) {
				iop->_flag |= _IOERR;
				return(EOF);
			}
			rn -= n;
			base += n;
		} while (rn > 0);
	}
	return(0);
}

fclose(iop)
	register FILE *iop;
{
	register int r;

	r = EOF;
	if (iop->_flag&(_IOREAD|_IOWRT|_IORW) && (iop->_flag&_IOSTRG)==0) {
		r = fflush(iop);
		if (close(fileno(iop)) < 0)
			r = EOF;
		if (iop->_flag&_IOMYBUF)
			free(iop->_base);
	}
	iop->_cnt = 0;
	iop->_base = (char *)NULL;
	iop->_ptr = (char *)NULL;
	iop->_bufsiz = 0;
	iop->_flag = 0;
	iop->_file = 0;
	return(r);
}

_cleanup()
{
	extern int _fwalk();

	_fwalk(fclose);
}
