/* @(#)clrerr.c	4.1 (Berkeley) 12/21/80 */
#include	<stdio.h>

clearerr(iop)
register struct _iobuf *iop;
{
	iop->_flag &= ~(_IOERR|_IOEOF);
}
