/* @(#)rewind.c	4.1 (Berkeley) %G% */
#include	<stdio.h>

rewind(iop)
register struct _iobuf *iop;
{
	fflush(iop);
	lseek(fileno(iop), 0L, 0);
	iop->_cnt = 0;
	iop->_ptr = iop->_base;
	iop->_flag &= ~(_IOERR|_IOEOF);
}
