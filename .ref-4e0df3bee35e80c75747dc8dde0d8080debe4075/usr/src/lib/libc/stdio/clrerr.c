/* @(#)clrerr.c	4.1 (Berkeley) %G% */
#include	<stdio.h>

clearerr(iop)
register struct _iobuf *iop;
{
	iop->_flag &= ~(_IOERR|_IOEOF);
}
