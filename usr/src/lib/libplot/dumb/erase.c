/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "dumb.h"

erase()
{
	register int i, j;

	for(i=0;i<COLS;i++)
		for(j=0;j<LINES;j++)
			screenmat[i][j] = ' ';
}
