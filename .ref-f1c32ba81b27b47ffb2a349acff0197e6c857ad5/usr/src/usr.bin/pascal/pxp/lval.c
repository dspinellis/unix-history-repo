/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lval.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

/*
 * A "variable"
 */
lvalue(r)
	register int *r;
{
	register *c, *co;

	ppid(r[2]);
	for (c = r[3]; c != NIL; c = c[2]) {
		co = c[1];
		if (co == NIL)
			continue;
		switch (co[0]) {
			case T_PTR:
				ppop("^");
				continue;
			case T_ARY:
				arycod(co[1]);
				continue;
			case T_FIELD:
				ppop(".");
				ppid(co[1]);
				continue;
			case T_ARGL:
				ppid("{unexpected argument list}");
				break;
			default:
				panic("lval2");
		}
	}
}

/*
 * Subscripting
 */
arycod(el)
	register int *el;
{

	ppbra("[");
	if (el != NIL)
		for (;;) {
			rvalue(el[1], NIL);
			el = el[2];
			if (el == NIL)
				break;
			ppsep(", ");
		}
	else
		rvalue(NIL, NIL);
	ppket("]");
}
