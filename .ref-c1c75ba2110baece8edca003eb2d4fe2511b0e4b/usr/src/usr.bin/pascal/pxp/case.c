/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)case.c	5.1 (Berkeley) %G%";
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
 * Case statement
 *	r	[0]	T_CASE
 *		[1]	lineof "case"
 *		[2]	expression
 *		[3]	list of cased statements:
 *			cstat	[0]	T_CSTAT
 *				[1]	lineof ":"
 *				[2]	list of constant labels
 *				[3]	statement
 */
caseop(r)
	int *r;
{
	register *cl, *cs, i;
	struct pxcnt scnt;
#	ifdef RMOTHERS
	    int *othersp;		/* tree where others is, or NIL */
	    int hasothers;		/* 1 if others found, else 0 */
#	endif RMOTHERS

#	ifdef RMOTHERS
	    if (rmothers) {
		hasothers = needscaseguard(r,&othersp);
		if (hasothers) {
		    precaseguard(r);
		}
	    }
#	endif RMOTHERS
	savecnt(&scnt);
	ppkw("case");
	ppspac();
	rvalue(r[2], NIL);
	ppspac();
	ppkw("of");
	for (cl = r[3]; cl != NIL;) {
		cs = cl[1];
		if (cs == NIL)
			continue;
		baroff();
		ppgoin(DECL);
		setline(cs[1]);
		ppnl();
		indent();
		ppbra(NIL);
		cs = cs[2];
		if (cs != NIL) {
			i = 0;
			for (;;) {
				gconst(cs[1]);
				cs = cs[2];
				if (cs == NIL)
					break;
				i++;
				if (i == 7) {
					ppsep(",");
					ppitem();
					i = 0;
				} else
					ppsep(", ");
			}
		} else
			ppid("{case label list}");
		ppket(":");
		cs = cl[1];
		cs = cs[3];
		getcnt();
		ppgoin(STAT);
		if (cs != NIL && cs[0] == T_BLOCK) {
			ppnl();
			indent();
			baron();
			ppstbl1(cs, STAT);
			baroff();
			ppstbl2();
			baron();
		} else {
			baron();
			statement(cs);
		}
		ppgoout(STAT);
		ppgoout(DECL);
		cl = cl[2];
		if (cl == NIL)
			break;
		ppsep(";");
	}
	if (rescnt(&scnt))
		getcnt();
	ppnl();
	indent();
	ppkw("end");
#	ifdef RMOTHERS
	    if (rmothers) {
		if (hasothers) {
		    postcaseguard(othersp);
		}
	    }
#	endif RMOTHERS
}
