#ifndef lint
static char sccsid[] = "@(#)rewind.c	5.1 (Berkeley) %G%";
#endif not lint

#include	<stdio.h>

rewind(iop)
register FILE *iop;
{
	fflush(iop);
	lseek(fileno(iop), 0L, 0);
	iop->_cnt = 0;
	iop->_ptr = iop->_base;
	iop->_flag &= ~(_IOERR|_IOEOF);
	if (iop->_flag & _IORW)
		iop->_flag &= ~(_IOREAD|_IOWRT);
}
