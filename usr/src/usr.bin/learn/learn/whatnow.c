/*-
 * Copyright (c) 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)whatnow.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "stdio.h"
#include "lrnref.h"

extern	char	togo[];
extern	int	review;

whatnow()
{
	if (again) {
		if (!review)
			printf("\nOK.  That was lesson %s.\n\n", todo);
		fflush(stdout);
		strcpy(level, togo);
		return;
	}
	if (skip) {
		printf("\nOK.  That was lesson %s.\n", todo);
		printf("Skipping to next lesson.\n\n");
		fflush(stdout);
		strcpy(level, todo);
		skip = 0;
		return;
	}
	if (todo == 0) {
		more=0;
		return;
	}
	if (didok) {
		strcpy(level,todo);
		if (speed<=9) speed++;
	}
	else {
		speed -= 4;
		/* the 4 above means that 4 right, one wrong leave
		    you with the same speed. */
		if (speed <0) speed=0;
	}
	if (wrong) {
		speed -= 2;
		if (speed <0 ) speed = 0;
	}
	if (didok && more) {
		printf("\nGood.  That was lesson %s.\n\n",level);
		fflush(stdout);
	}
}
