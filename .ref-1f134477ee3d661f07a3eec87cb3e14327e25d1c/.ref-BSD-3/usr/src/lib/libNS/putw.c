/* Copyright (c) 1979 Regents of the University of California */
#include	<stdio.h>

putw(i, iop)
register i;
register struct _iobuf *iop;
{
	putc(i, iop);
	putc(i>>=8, iop);
	putc(i>>=8, iop);
	putc(i>>=8, iop);
}
short putsh(i, iop)
register i;
register struct _iobuf *iop;
{
	putc(i, iop);
	putc(i>>8, iop);
}
