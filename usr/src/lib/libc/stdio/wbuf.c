/* @(#)wbuf.c	4.3 (Berkeley) %G% */
#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>

char	*malloc();

_flsbuf(c, iop)
register FILE *iop;
{
	register char *base;
	register n, rn;
	char c1;
	int size;
	struct stat stbuf;
	extern char _sobuf[];

	if (iop->_flag & _IORW) {
		iop->_flag |= _IOWRT;
		iop->_flag &= ~_IOEOF;
	}

	if ((iop->_flag&_IOWRT)==0)
		return(EOF);
tryagain:
	if (iop->_flag&_IOLBF) {
		base = iop->_base;
		*iop->_ptr++ = c;
		if (iop->_ptr >= base+iop->_bufsiz || c == '\n') {
			n = write(fileno(iop), base, rn = iop->_ptr - base);
			iop->_ptr = base;
		} else
			rn = n = 0;
		iop->_cnt = 0;
	} else if (iop->_flag&_IONBF) {
		c1 = c;
		rn = 1;
		n = write(fileno(iop), &c1, rn);
		iop->_cnt = 0;
	} else {
		if ((base=iop->_base)==NULL) {
			if (fstat(fileno(iop), &stbuf) < 0 ||
			    stbuf.st_blksize <= NULL)
				size = BUFSIZ;
			else
				size = stbuf.st_blksize;
			if (iop==stdout) {
				if (isatty(fileno(stdout)))
					iop->_flag |= _IOLBF;
				iop->_base = _sobuf;
				iop->_ptr = _sobuf;
				iop->_bufsiz = size;
				goto tryagain;
			}
			if ((iop->_base=base=malloc(size)) == NULL) {
				iop->_flag |= _IONBF;
				goto tryagain;
			}
			iop->_flag |= _IOMYBUF;
			iop->_bufsiz = size;
			rn = n = 0;
		} else if ((rn = n = iop->_ptr - base) > 0) {
			iop->_ptr = base;
			n = write(fileno(iop), base, n);
		}
		iop->_cnt = iop->_bufsiz-1;
		*base++ = c;
		iop->_ptr = base;
	}
	if (rn != n) {
		iop->_flag |= _IOERR;
		return(EOF);
	}
	return(c);
}

fflush(iop)
register struct _iobuf *iop;
{
	register char *base;
	register n;

	if ((iop->_flag&(_IONBF|_IOWRT))==_IOWRT
	 && (base=iop->_base)!=NULL && (n=iop->_ptr-base)>0) {
		iop->_ptr = base;
		iop->_cnt = (iop->_flag&(_IOLBF|_IONBF)) ? 0 : iop->_bufsiz;
		if (write(fileno(iop), base, n)!=n) {
			iop->_flag |= _IOERR;
			return(EOF);
		}
	}
	return(0);
}

/*
 * Flush buffers on exit
 */

_cleanup()
{
	register struct _iobuf *iop;
	extern struct _iobuf *_lastbuf;

	for (iop = _iob; iop < _lastbuf; iop++)
		fclose(iop);
}

fclose(iop)
register struct _iobuf *iop;
{
	register r;

	r = EOF;
	if (iop->_flag&(_IOREAD|_IOWRT|_IORW) && (iop->_flag&_IOSTRG)==0) {
		r = fflush(iop);
		if (close(fileno(iop)) < 0)
			r = EOF;
		if (iop->_flag&_IOMYBUF)
			free(iop->_base);
		if (iop->_flag&(_IOMYBUF|_IONBF|_IOLBF)) {
			iop->_base = NULL;
			iop->_bufsiz = 0;
		}
	}
	iop->_flag &= ~(_IOREAD|_IOWRT|_IOLBF|_IONBF|_IOMYBUF|_IOERR|_IOEOF|_IOSTRG|_IORW);
	iop->_cnt = 0;
	return(r);
}
