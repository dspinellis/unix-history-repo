#
/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
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
