/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)case.c	8.1 (Berkeley) 6/6/93";
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
