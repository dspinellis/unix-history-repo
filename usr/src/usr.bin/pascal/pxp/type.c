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
static char sccsid[] = "@(#)type.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

STATIC	int typecnt = -1;
/*
 * Type declaration part
 */
typebeg(l, tline)
	int l, tline;
{

	line = l;
	if (nodecl)
		printoff();
	puthedr();
	putcm();
	ppnl();
	indent();
	ppkw("type");
	ppgoin(DECL);
	typecnt = 0;
	setline(tline);
}

type(tline, tid, tdecl)
	int tline;
	char *tid;
	int *tdecl;
{

	if (typecnt)
		putcm();
	setline(tline);
	ppitem();
	ppid(tid);
	ppsep(" =");
	gtype(tdecl);
	ppsep(";");
	setinfo(tline);
	putcml();
	typecnt++;
}

typeend()
{

	if (typecnt == -1)
		return;
	if (typecnt == 0)
		ppid("{type decls}");
	ppgoout(DECL);
	typecnt = -1;
}

/*
 * A single type declaration
 */
gtype(r)
	register int *r;
{

	if (r == NIL) {
		ppid("{type}");
		return;
	}
	if (r[0] != T_ID && r[0] != T_TYPACK)
		setline(r[1]);
	switch (r[0]) {
		default:
			panic("type");
		case T_ID:
			ppspac();
			ppid(r[1]);
			return;
		case T_TYID:
			ppspac();
			ppid(r[2]);
			break;
		case T_TYSCAL:
			ppspac();
			tyscal(r);
			break;
		case T_TYCRANG:
			ppspac();
			tycrang(r);
			break;
		case T_TYRANG:
			ppspac();
			tyrang(r);
			break;
		case T_TYPTR:
			ppspac();
			ppop("^");
			gtype(r[2]);
			break;
		case T_TYPACK:
			ppspac();
			ppkw("packed");
			gtype(r[2]);
			break;
		case T_TYCARY:
		case T_TYARY:
			ppspac();
			tyary(r);
			break;
		case T_TYREC:
			ppspac();
			tyrec(r[2], NIL);
			break;
		case T_TYFILE:
			ppspac();
			ppkw("file");
			ppspac();
			ppkw("of");
			gtype(r[2]);
			break;
		case T_TYSET:
			ppspac();
			ppkw("set");
			ppspac();
			ppkw("of");
			gtype(r[2]);
			break;
	}
	setline(r[1]);
	putcml();
}

/*
 * Scalar type declaration
 */
tyscal(r)
	register int *r;
{
	register int i;

	ppsep("(");
	r = r[2];
	if (r != NIL) {
		i = 0;
		ppgoin(DECL);
		for (;;) {
			ppid(r[1]);
			r = r[2];
			if (r == NIL)
				break;
			ppsep(", ");
			i++;
			if (i == 7) {
				ppitem();
				i = 0;
			}
		}
		ppgoout(DECL);
	} else
		ppid("{constant list}");
	ppsep(")");
}

/*
 * Conformant array subrange.
 */
tycrang(r)
	register int *r;
{

	ppid(r[2]);
	ppsep("..");
	ppid(r[3]);
	ppsep(":");
	gtype(r[4]);
}

/*
 * Subrange type declaration
 */
tyrang(r)
	register int *r;
{

	gconst(r[2]);
	ppsep("..");
	gconst(r[3]);
}

/*
 * Array type declaration
 */
tyary(r)
	register int *r;
{
	register int *tl;

	ppkw("array");
	ppspac();
	ppsep("[");
	tl = r[2];
	if (tl != NIL) {
		ppunspac();
		for (;;) {
			gtype(tl[1]);
			tl = tl[2];
			if (tl == NIL)
				break;
			ppsep(",");
		}
	} else
		ppid("{subscr list}");
	ppsep("]");
	ppspac();
	ppkw("of");
	gtype(r[3]);
}
