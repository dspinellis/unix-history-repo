/* Copyright (c) 1979 Regents of the University of California */
#include	<stdio.h>

_flsbuf(c, iop)
register struct _iobuf *iop;
{
	register n;
	register char *base;
	char c1, *malloc();
	extern char _sobuf[];

	if ((iop->_flag&_IOWRT)==0)
		_error("writing\n");
tryagain:
	if (iop->_flag&_IONBF) {
		c1 = c;
		n = write(fileno(iop), &c1, 1);
		iop->_cnt = 0;
	} else {
		if ((base=iop->_base)==NULL) {
			if (iop==stdout) {
				iop->_base = _sobuf;
				iop->_ptr = _sobuf;
				goto tryagain;
			}
			if ((iop->_base=base=malloc(BUFSIZ)) == NULL) {
				iop->_flag |= _IONBF;
				goto tryagain;
			}
			iop->_flag |= _IOMYBUF;
			n = 1;
		} else if ((n = iop->_ptr - base) > 0) {
			if (iop->_delta && (iop->_flag&_IOREAD)) {
				if(lseek(iop->_file, (long) -iop->_delta, 1)<0)
					_error("Seek error in flsbuf\n");
				iop->_delta = 0;
			}
			n = write(fileno(iop), base, n);
			if ((iop->_flag & _IOREAD) &&
			 (0 >(iop->_delta = read(iop->_file, base, BUFSIZ)))) {
				iop->_delta = 0;
				iop->_flag |= _IOERR;
			}
		}
		iop->_cnt = BUFSIZ-1;
		*base++ = c;
		iop->_ptr = base;
	}
	if (n < 0) {
		iop->_flag |= _IOERR;
		return(-1);
	}
	return(c);
}

fflush(iop)
register struct _iobuf *iop;
{
	register char *base;
	register n;

	if ((iop->_flag&(_IONBF|_IOWRT))==_IOWRT
	 && (base=iop->_base)!=NULL && ((n=iop->_ptr-base)>0) 
	 && (iop->_flag & _IODIRT)) {
		iop->_ptr = base;
		iop->_cnt = 0;
		if(iop->_delta) {
			if(0>lseek(fileno(iop), (long) -iop->_delta, 1)) {
				_error("Seek error in fflush\n");
				iop->_flag |= _IOERR;
			}
			iop->_delta = 0;
		}
		if (write(fileno(iop), base, n)!=n)
			iop->_flag |= _IOERR;
		iop->_flag &= ~_IODIRT;
	}
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
	if (iop->_flag&(_IOREAD|_IOWRT)) {
		fflush(iop);
		close(fileno(iop));
		if (iop->_flag&_IOMYBUF)
			free(iop->_base);
	}
	iop->_base = NULL;
	iop->_flag &= ~(_IOREAD|_IOWRT|_IONBF|_IOMYBUF|_IOERR|_IOEOF|_IODIRT|_IOSTRG);
	iop->_cnt = 0;
	iop->_delta = 0;
	return(0);
}
