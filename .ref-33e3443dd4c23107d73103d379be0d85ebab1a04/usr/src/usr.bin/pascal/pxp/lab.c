/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)lab.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"

/*
 * Label declaration part
 */
label(r, l)
	int *r, l;
{
	register *ll;

	if (nodecl)
		printoff();
	puthedr();
	setline(l);
	ppnl();
	indent();
	ppkw("label");
	ppgoin(DECL);
	ppnl();
	indent();
	ppbra(NIL);
	ll = r;
	if (ll != NIL)
		for (;;) {
			pplab(ll[1]);
			ll = ll[2];
			if (ll == NIL)
				break;
			ppsep(", ");
		}
	else
		ppid("{label list}");
	ppket(";");
	putcml();
	ppgoout(DECL);
}

/*
 * Goto statement
 */
gotoop(s)
	char *s;
{

	gocnt++;
	ppkw("goto");
	ppspac();
	pplab(s);
}

/*
 * A label on a statement
 */
labeled(s)
	char *s;
{

	linopr();
	indentlab();
	pplab(s);
	ppsep(":");
}
