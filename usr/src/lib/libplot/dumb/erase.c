#ifndef lint
static char sccsid[] = "@(#)erase.c	4.1 (Berkeley) %G%";
#endif

#include "dumb.h"

erase()
{
	register int i, j;

	for(i=0;i<COLS;i++)
		for(j=0;j<LINES;j++)
			screenmat[i][j] = ' ';
}
