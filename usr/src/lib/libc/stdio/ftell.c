#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ftell.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

/*
 * Return file offset.
 * Coordinates with buffering.
 */

#include	<stdio.h>
long	lseek();


long ftell(iop)
register FILE *iop;
{
	register long tres;
	register adjust;

	if (iop->_cnt < 0)
		iop->_cnt = 0;
	if (iop->_flag&_IOREAD)
		adjust = - iop->_cnt;
	else if (iop->_flag&(_IOWRT|_IORW)) {
		adjust = 0;
		if (iop->_flag&_IOWRT && iop->_base && (iop->_flag&_IONBF)==0)
			adjust = iop->_ptr - iop->_base;
	} else
		return(-1);
	tres = lseek(fileno(iop), 0L, 1);
	if (tres<0)
		return(tres);
	tres += adjust;
	return(tres);
}
