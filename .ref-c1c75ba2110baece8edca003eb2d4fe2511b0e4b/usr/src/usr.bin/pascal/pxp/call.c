/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)call.c	5.1 (Berkeley) %G%";
#endif not lint

#
/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

/*
 * Procedure or function call
 */
call(p, argv)
	register int *argv;
{
	register *al;

	ppid(p);
	if (argv != NIL) {
		ppbra("(("+1);	/* xaproposstrange */
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
