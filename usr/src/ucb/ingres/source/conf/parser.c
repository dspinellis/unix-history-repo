# include	<ingres.h>
# include	<aux.h>
# include	<access.h>
# include	<func.h>
# include	<sccs.h>

SCCSID(@(#)parser.c	7.1	2/5/81)



char	Qbuf[2000];
int	QbufSize = sizeof Qbuf;

extern struct fn_def	ParserFn;

DESC	Attdes;


struct desxx	Desxx[] =
{
	"attribute",	&Attdes,	&Admin.adattd,
	NULL
};

struct fn_def	*FuncVect[] =
{
	&ParserFn
};

int	NumFunc = sizeof FuncVect / sizeof FuncVect[0];
