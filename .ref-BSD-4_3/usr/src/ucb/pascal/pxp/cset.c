/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cset.c	5.1 (Berkeley) 6/5/85";
#endif not lint

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

/*
 * Constant sets
 */
cset(r)
int *r;
{
	register *e, *el;

	ppbra("[");
	el = r[2];
	if (el != NIL)
		for (;;) {
			e = el[1];
			el = el[2];
			if (e == NIL)
				continue;
			if (e[0] == T_RANG) {
				rvalue(e[1], NIL);
				ppsep("..");
				rvalue(e[2], NIL);
			} else
				rvalue(e, NIL);
			if (el == NIL)
				break;
			ppsep(", ");
		}
	ppket("]");
}
