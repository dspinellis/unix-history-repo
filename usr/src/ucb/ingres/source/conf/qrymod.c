# include	<func.h>
# include	<sccs.h>

SCCSID(@(#)qrymod.c	7.1	2/5/81)



char	Qbuf[400];
int	QbufSize = sizeof Qbuf;

extern struct fn_def	QryModFn;
extern struct fn_def	DefProFn;
extern struct fn_def	DefIntFn;
extern struct fn_def	DefViewFn;

struct fn_def	*FuncVect[] =
{
	&QryModFn,
	&DefViewFn,
	&DefIntFn,
	&DefProFn,
};

int	NumFunc = sizeof FuncVect / sizeof FuncVect[0];
