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
static char sccsid[] = "@(#)rec.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"

tyrec(r, p0)
	int *r, p0;
{

	if (r != NIL)
		setinfo(r[1]);
	if (p0 == NIL) {
		ppgoin(DECL);
		ppnl();
		indent();
		ppkw("record");
		ppspac();
	} else {
		ppspac();
		ppbra("(");
	}
	ppgoin(DECL);
	if (r) {
		field(r[2], r[3]);
		variant(r[3]);
	}
	if (r != NIL)
		setinfo(r[1]);
	putcml();
	ppgoout(DECL);
	if (p0 == NIL) {
		ppnl();
		indent();
		ppkw("end");
		ppgoout(DECL);
	} else {
		ppitem();
		ppket(")");
	}
}

field(r, v)
	int *r, *v;
{
	register int *fp, *tp, *ip;

	fp = r;
	if (fp != NIL)
		for (;;) {
			tp = fp[1];
			if (tp != NIL) {
				setline(tp[1]);
				ip = tp[2];
				ppitem();
				if (ip != NIL)
					for (;;) {
						ppid(ip[1]);
						ip = ip[2];
						if (ip == NIL)
							break;
						ppsep(", ");
					}
				else
					ppid("{field id list}");
				ppsep(":");
				gtype(tp[3]);
				setinfo(tp[1]);
				putcm();
			}
			fp = fp[2];
			if (fp == NIL)
				break;
			ppsep(";");
		}
	if (v != NIL && r != NIL)
		ppsep(";");
}

variant(r)
	register int *r;
{
	register int *v, *vc;

	if (r == NIL)
		return;
	setline(r[1]);
	ppitem();
	ppkw("case");
	v = r[2];
	if (v != NIL) {
		ppspac();
		ppid(v);
		ppsep(":");
	}
	gtype(r[3]);
	ppspac();
	ppkw("of");
	for (vc = r[4]; vc != NIL;) {
		v = vc[1];
		if (v == NIL)
			continue;
		ppgoin(DECL);
		setline(v[1]);
		ppnl();
		indent();
		ppbra(NIL);
		v = v[2];
		if (v != NIL) {
			for (;;) {
				gconst(v[1]);
				v = v[2];
				if (v == NIL)
					break;
				ppsep(", ");
			}
		} else
			ppid("{case label list}");
		ppket(":");
		v = vc[1];
		tyrec(v[3], 1);
		setinfo(v[1]);
		putcml();
		ppgoout(DECL);
		vc = vc[2];
		if (vc == NIL)
			break;
		ppsep(";");
	}
	setinfo(r[1]);
	putcm();
}
