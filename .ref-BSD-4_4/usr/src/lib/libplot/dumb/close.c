/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include <signal.h>
#include "dumb.h"

void
closepl()
{
	int i, j;

	for(j=0; j<LINES; j++){
		for(i=0; i<COLS; i++){
			printf("%c", screenmat[i][j]);
		}
		printf("\n");
	}
	signal(SIGINT, SIG_IGN);
	exit(0);
}
