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
 * Procedure or function call
 */
call(p, argv)
	int *argv;
{
	register *al;

	ppid(p);
	if (argv != NIL) {
		ppbra("(");
		for (;;) {
			al = argv[1];
			if (al[0] == T_WEXP) {
				rvalue(al[1], NIL);
				if (al[2] != NIL) {
					ppsep(": ");
					rvalue(al[2], NIL);
				}
				if (al[3] == OCT || al[3] == HEX) {
					ppspac();
					ppkw(al[3] == OCT ? "oct" : "hex");
				} else if (al[3] != NIL) {
					ppsep(": ");
					rvalue(al[3], NIL);
				}
			} else
				rvalue(argv[1], NIL);
			argv = argv[2];
			if (argv == NIL)
				break;
			ppsep(", ");
		}
		ppket(")");
	}
}
