#ifndef lint
static	char *sccsid = "@(#)wwdata.c	3.10 84/03/23";
#endif

#include "ww.h"
#include "tt.h"

int tt_h19();
int tt_f100();
int tt_generic();
struct tt_tab tt_tab[] = {
	{ "h19",	3, tt_h19 },
	{ "f100",	4, tt_f100 },
	{ "generic",	0, tt_generic },
	0
};

int wwbaudmap[] = {
	0,
	50,
	75,
	110,
	134,
	150,
	200,
	300,
	600,
	1200,
	1800,
	2400,
	4800,
	9600,
	19200,
	38400
};
