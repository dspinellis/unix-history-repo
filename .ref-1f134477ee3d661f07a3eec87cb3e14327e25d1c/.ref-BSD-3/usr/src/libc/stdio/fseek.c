/*
 * Seek for standard library.  Coordinates with buffering.
 */

#include	<stdio.h>

long lseek();

fseek(iop, offset, ptrname)
FILE *iop;
long offset;
{
	register resync, c;
	long p;

	iop->_flag &= ~_IOEOF;
	if (iop->_flag&_IOREAD) {
		if (ptrname<2 && iop->_base &&
			!(iop->_flag&_IONBF)) {
			c = iop->_cnt;
			p = offset;
			if (ptrname==0)
				p += c - lseek(fileno(iop),0L,1);
			else
				offset -= c;
			if(c>0&&p<=c&&p>=iop->_base-iop->_ptr){
				iop->_ptr += (int)p;
				iop->_cnt -= (int)p;
				return(0);
			}
			resync = offset&01;
		} else 
			resync = 0;
		p = lseek(fileno(iop), offset-resync, ptrname);
		iop->_cnt = 0;
		if (resync)
			getc(iop);
	}
	else if (iop->_flag&_IOWRT) {
		fflush(iop);
		p = lseek(fileno(iop), offset, ptrname);
	}
	return(p==-1?-1:0);
}
