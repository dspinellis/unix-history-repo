/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)const.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

STATIC	int constcnt = -1;

/*
 * The const declaration part
 */
constbeg(l, cline)
	int l, cline;
{

	line = l;
	if (nodecl)
		printoff();
	puthedr();
	putcm();
	ppnl();
	indent();
	ppkw("const");
	ppgoin(DECL);
	constcnt = 0;
	setline(cline);
}

const(cline, cid, cdecl)
	int cline;
	char *cid;
	int *cdecl;
{

	if (constcnt)
		putcm();
	setline(cline);
	ppitem();
	ppid(cid);
	ppsep(" = ");
	gconst(cdecl);
	ppsep(";");
	constcnt++;
	setinfo(cline);
	putcml();
}

constend()
{

	if (constcnt == -1)
		return;
	if (nodecl)
		return;
	if (constcnt == 0)
		ppid("{const decls}");
	ppgoout(DECL);
	constcnt = -1;
}

/*
 * A constant in an expression
 * or a declaration.
 */
gconst(r)
	int *r;
{
	register *cn;

	cn = r;
loop:
	if (cn == NIL) {
		ppid("{constant}");
		return;
	}
	switch (cn[0]) {
		default:
			panic("gconst");
		case T_PLUSC:
			ppop("+");
			cn = cn[1];
			goto loop;
		case T_MINUSC:
			ppop("-");
			cn = cn[1];
			goto loop;
		case T_ID:
			ppid(cn[1]);
			return;
		case T_CBINT:
		case T_CINT:
		case T_CFINT:
			ppnumb(cn[1]);
			if (cn[0] == T_CBINT)
				ppsep("b");
			return;
		case T_CSTRNG:
			ppstr(cn[1]);
			return;
	}
}
