/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)draw.c	5.1 (Berkeley) 4/26/85";
#endif not lint

/*
 * draw a line from the current place to (x,y).  Such lines are
 * supposed to be horizontal, and are affected by the current mode.
 */

#include "2648.h"

draw(x, y)
{
#ifdef TRACE
	if (trace) {
		fprintf(trace, "draw(%d,%d)\n", x, y);
	}
#endif
	sync();
	escseq(ESCP);
	motion(x, y);
	_supx = x;
	_supy = y;
}
