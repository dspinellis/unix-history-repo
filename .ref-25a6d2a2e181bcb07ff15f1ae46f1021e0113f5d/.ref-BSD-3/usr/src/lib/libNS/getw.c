/* Copyright (c) 1979 Regents of the University of California */
#include	<stdio.h>

short getsh(iop)
register struct _iobuf *iop;
{
	register i;

	i = getc(iop);
	if (iop->_flag&_IOEOF)
		return(-1);
	return(i | (getc(iop)<<8));
}
getw(iop)
register struct _iobuf *iop;
{
	register i;

	i = getsh(iop);
	if (iop->_flag&_IOEOF)
		return(-1);
	return(i | (getsh(iop)<<16));
}
