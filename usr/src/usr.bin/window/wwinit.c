#ifndef lint
static	char *sccsid = "@(#)wwinit.c	1.1 83/07/12";
#endif

#include "ww.h"

int _wwiflag = 0;

wwinit()
{
	if (_wwiflag)
		return;
	_wwiflag++;
	initscr();
}
