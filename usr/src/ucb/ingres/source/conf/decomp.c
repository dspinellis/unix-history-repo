# include	<func.h>
# include	<sccs.h>

SCCSID(@(#)decomp.c	7.1	2/5/81)




char Qbuf[1000];
int QbufSiz = sizeof Qbuf;

extern struct fn_def	DeOvqpFn;

struct fn_def	*FuncVect[] =
{
	&DeOvqpFn,
};

int	NumFunc = sizeof FuncVect / sizeof FuncVect[0];
