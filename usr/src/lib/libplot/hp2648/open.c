/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)open.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include <sgtty.h>
#include "hp2648.h"

int shakehands;
int currentx;
int currenty;
int buffcount;
int fildes;
float lowx;
float lowy;
float scalex;
float scaley;
struct sgttyb sarg;

openpl()
{
	if ( isatty(fileno( stdout )) ) {
		shakehands = TRUE;
		fildes = open(TERMINAL, 0);
		gtty(fildes, &sarg);
		sarg.sg_flags = sarg.sg_flags | RAW;
		stty(fildes, &sarg);
		sarg.sg_flags = sarg.sg_flags & ~RAW;
	}
	else {
		shakehands = FALSE;
	}
	buffcount = 0;
	currentx = 0;
	currenty = 0;
	buffready(8);
	putchar(ESC);
	putchar(GRAPHIC);
	putchar(DISPLAY);
	putchar('c');
	putchar(ESC);
	putchar(GRAPHIC);
	putchar(PLOT);
	putchar(BINARY);
	space(0,0,720,360);
}
