/*
 * Seek for standard library.  Coordinates with buffering.
 */

#include	<stdio.h>

long lseek();

fseek(iop, offset, ptrname)
	register FILE *iop;
	long offset;
{
	register int c;
	long p;

	iop->_flag &= ~_IOEOF;
	if (iop->_flag & _IOREAD) {
		if (ptrname < 2 && iop->_base && !(iop->_flag&_IONBF)) {
			c = iop->_cnt;
			p = offset;
			if (ptrname == 0)
				p += c - lseek(fileno(iop), 0L, 1);
			else
				offset -= c;
			if (!(iop->_flag&_IORW) && c > 0 && p <= c
			    && p >= iop->_base - iop->_ptr){
				iop->_ptr += (int) p;
				iop->_cnt -= (int) p;
				return(0);
			}
		}
		if (iop->_flag & _IORW) {
			iop->_ptr = iop->_base;
			iop->_flag &= ~_IOREAD;
		}
		p = lseek(fileno(iop), offset, ptrname);
		iop->_cnt = 0;
	} else if(iop->_flag & (_IOWRT|_IORW)) {
		fflush(iop);
		if (iop->_flag & _IORW) {
			iop->_cnt = 0;
			iop->_flag &= ~_IOWRT;
			iop->_ptr = iop->_base;
		}
		p = lseek(fileno(iop), offset, ptrname);
	}
	return(p==-1? -1: 0);
}
