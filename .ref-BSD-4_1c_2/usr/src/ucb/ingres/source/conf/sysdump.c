# include	<func.h>
# include	<sccs.h>

SCCSID(@(#)sysdump.c	7.1	2/5/81)



char	Qbuf[400];
int	QbufSize = sizeof Qbuf;

extern struct fn_def	SysDmpFn;

struct fn_def	*FuncVect[] =
{
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
};

int	NumFunc = sizeof FuncVect / sizeof FuncVect[0];
