/* Copyright (c) 1979 Regents of the University of California */
#include	<stdio.h>

/*
 * Seek for standard library.  Coordinates with buffering.
 */

long fseek(iop, offset, ptrname)
register FILE *iop;
long offset;
{
	register n, resync;

	if (iop->_flag&_IODIRT) {
		fflush(iop);
		return(lseek(fileno(iop), offset, ptrname));
	}
	if (iop->_flag&(_IOREAD|_IOWRT)) {
		resync = 0;
		if (ptrname==1) {	/* relative */
			n = iop->_cnt;
			if (n<0)
				n = 0;
		} else {
			n = offset&01;
			resync = n;
		}
		n = lseek(fileno(iop), offset - n, ptrname);
		iop->_cnt = 0;
		iop->_ptr = iop->_base ;
		iop->_delta = 0;
		if (resync)
			getc(iop);
		return(n);
	}
	_error("fseek\n");
}

/*  The current character is always iop->_cnt characters behind the current 
position in the file, except when a file is open and in use for pure writing
in which case it is _IODIRT(y), and will be correctly positioned by fflush */
