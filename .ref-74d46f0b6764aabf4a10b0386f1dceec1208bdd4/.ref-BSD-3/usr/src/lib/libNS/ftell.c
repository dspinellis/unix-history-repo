/* Copyright (c) 1979 Regents of the University of California */
#include	<stdio.h>

/*
 * Return file offset.
 * Coordinates with buffering.
 */

long	tell();

long ftell(iop)
register FILE *iop;
{
	long tres;
	register adjust;

	if (iop->_cnt < 0)
		iop->_cnt = 0;
	if (!(iop->_flag & (_IOREAD|_IOWRT)))
		_error("ftell\n");
	tres = tell(fileno(iop));
	if (tres<0)
		return(tres);
	return(tres - iop->_delta + iop->_ptr - iop->_base);
}
