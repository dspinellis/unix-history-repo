# include	<func.h>
# include	<sccs.h>

SCCSID(@(#)monitor.c	7.1	2/5/81)



char	Qbuf[400];
int	QbufSize = sizeof Qbuf;

extern struct fn_def	TtyMonFn;

struct fn_def	*FuncVect[] =
{
	&TtyMonFn,
};

int	NumFunc = sizeof FuncVect / sizeof FuncVect[0];
