/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)motion.c	5.1 (Berkeley) 4/30/85";
#endif not lint

/*
 * Move the pen to x, y.  We assume we are already in ESCP mode.
 */

#include "2648.h"

motion(x, y)
{
	char lox, loy, hix, hiy;
	int delx, dely;

	delx = x-_penx; dely = y-_peny;
	if (-16 <= delx && delx <= 15 && -16 <= dely && dely <= 15) {
		/*
		 * Optimization: if within 15 in both directions, can use
		 * HP short incremental mode, only 3 bytes.
		 */
		outchar('j');
		outchar(32 + (delx & 31));
		outchar(32 + (dely & 31));
	} else {
		/*
		 * Otherwise must use binary absolute mode, 5 bytes.
		 * We never use ascii mode or binary incremental, since
		 * those both take many more bytes.
		 */
		outchar('i');
		outchar(32+ ((x>>5) & 31));
		outchar(32+ (x&31));
		outchar(32+ ((y>>5) & 31));
		outchar(32+ (y&31));
	}
	_penx = x;
	_peny = y;
}
