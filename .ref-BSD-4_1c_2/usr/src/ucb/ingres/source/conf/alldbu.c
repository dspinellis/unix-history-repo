# include	<func.h>
# include	<sccs.h>

SCCSID(@(#)alldbu.c	7.1	2/5/81)



char	Qbuf[1500];
int	QbufSize = sizeof Qbuf;

int	Noupdt;
short	tTdbu[100];

extern struct fn_def	CopyFn;
extern struct fn_def	CreateFn;
extern struct fn_def	DstroyFn;
extern struct fn_def	HelpFn;
extern struct fn_def	DsplayFn;
/* extern struct fn_def	KsortFn; */
extern struct fn_def	ModifyFn;
extern struct fn_def	PrintFn;
extern struct fn_def	ResetrFn;
extern struct fn_def	RmqmFn;
extern struct fn_def	RupdatFn;
extern struct fn_def	SaveFn;
extern struct fn_def	IndexFn;

struct fn_def	*FuncVect[] =
{
	/* 0 -- create */	&CreateFn,
	/* 1 -- destroy */	&DstroyFn,
	/* 2 -- update */	&RupdatFn,
	/* 3 -- print */	&PrintFn,
	/* 4 -- help */		&HelpFn,
	/* 5 -- resetrel */	&ResetrFn,
	/* 6 -- copy */		&CopyFn,
	/* 7 -- save */		&SaveFn,
	/* 8 -- modify */	&ModifyFn,
	/* 9 -- index */	&IndexFn,
	/* 10 -- display */	&DsplayFn,
	/* 11 -- unused */	&PrintFn,
	/* 12 -- remqm */	&RmqmFn,
};

int	NumFunc = sizeof FuncVect / sizeof FuncVect[0];
