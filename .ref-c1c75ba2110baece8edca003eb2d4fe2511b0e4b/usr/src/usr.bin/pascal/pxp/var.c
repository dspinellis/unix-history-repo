/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)var.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

STATIC	int varcnt = -1;
/*
 * Var declaration part
 */
varbeg(l, vline)
	int l, vline;
{

	line = l;
	if (nodecl)
		printoff();
	puthedr();
	putcm();
	ppnl();
	indent();
	ppkw("var");
	ppgoin(DECL);
	varcnt = 0;
	setline(vline);
}

var(vline, vidl, vtype)
	int vline;
	register int *vidl;
	int *vtype;
{

	if (varcnt)
		putcm();
	setline(vline);
	ppitem();
	if (vidl != NIL)
		for (;;) {
			ppid(vidl[1]);
			vidl = vidl[2];
			if (vidl == NIL)
				break;
			ppsep(", ");
		}
	else
		ppid("{identifier list}");
	ppsep(":");
	gtype(vtype);
	ppsep(";");
	setinfo(vline);
	putcml();
	varcnt++;
}

varend()
{

	if (varcnt == -1)
		return;
	if (varcnt == 0)
		ppid("{variable decls}");
	ppgoout(DECL);
	varcnt = -1;
}
