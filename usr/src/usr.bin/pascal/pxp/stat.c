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
static char sccsid[] = "@(#)stat.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

int cntstat;
int cnts = 3;

statlist(r)
	int *r;
{
	register int *sl;

	sl = r;
	if (sl != NIL)
		for (;;) {
			statement(sl[1]);
			sl = sl[2];
			if (sl == NIL)
				break;
			ppsep(";");
		}
	else
		statement(NIL);
}


statement(r)
	int *r;
{
	register int *s;

	s = r;
top:
	if (cntstat) {
		cntstat = 0;
		getcnt();
	}
	if (s == NIL) {
		putcm();
		ppitem();
		ppid("null");
		return;
	}
	if (s[0] == T_REPEAT)
		setinfo(s[1]);
	else
		setline(s[1]);
	if (s[0] == T_LABEL) {
		cntstat = 1;
		ppnl();
		labeled(s[2]);
		statement(s[3]);
		return;
	}
	switch (s[0]) {
		default:
			panic("stat");
		case T_PCALL:
			ppitem();
			proc(s);
			break;
		case T_IF:
		case T_IFEL:
			ppnl();
			indent();
			ifop(s);
			break;
		case T_WHILE:
			ppnl();
			indent();
			whilop(s);
			break;
		case T_REPEAT:
			ppnl();
			indent();
			repop(s);
			break;
		case T_FORU:
		case T_FORD:
			ppnl();
			indent();
			forop(s);
			break;
		case T_BLOCK:
			ppnl();
			indent();
			ppstbl(s, DECL);
			break;
		case T_ASGN:
			ppitem();
			asgnop(s);
			break;
		case T_GOTO:
			ppitem();
			gotoop(s[2]);
			cntstat = 1;
			break;
		case T_CASE:
			ppnl();
			indent();
			caseop(s);
			break;
		case T_WITH:
			ppnl();
			indent();
			withop(s);
			break;
	}
	setinfo(s[1]);
	putcm();
}

withop(s)
	int *s;
{
	register *p;

	ppkw("with");
	ppspac();
	p = s[2];
	if (p != NIL)
		for (;;) {
			lvalue(p[1]);
			p = p[2];
			if (p == NIL)
				break;
			ppsep(", ");
		}
	else
		ppid("{record variable list}");
	ppstdo(s[3], DECL);
}

asgnop(r)
	int *r;
{

	lvalue(r[2]);
	ppsep(" := ");
	rvalue(r[3], NIL);
}

forop(r)
	int *r;
{
	struct pxcnt scnt;

	savecnt(&scnt);
	ppkw("for");
	ppspac();
	asgnop(r[2]);
	ppspac();
	ppkw(r[0] == T_FORU ? "to" : "downto");
	ppspac();
	rvalue(r[3], NIL);
	getcnt();
	ppstdo(r[4], STAT);
	if (rescnt(&scnt))
		getcnt();
}

ifop(r)
	int *r;
{
	register *s;
	struct pxcnt scnt;

	ppkw("if");
	ppspac();
	rvalue(r[2], NIL);
	ppspac();
	ppkw("then");
	ppspac();
	s = r[3];
	savecnt(&scnt);
	getcnt();
	if (s != NIL && s[0] == T_BLOCK)
		ppstbl1(s, STAT);
	else {
		ppgoin(STAT);
		statement(s);
		ppgoout(STAT);
	}
	if (r[0] == T_IFEL) {
		setcnt(cntof(&scnt)-nowcnt());
		if (s == NIL || s[0] != T_BLOCK) {
			ppnl();
			indent();
		} else {
			ppstbl2();
			ppspac();
		}
		s = r[4];
		ppkw("else");
		unprint();
		ppspac();
		if (s == NIL)
			goto burp;
		if (s[0] == T_BLOCK)
			ppstbl1(s, STAT);
		else if (s[0] == T_IF || s[0] == T_IFEL)
			ifop(s);
		else {
burp:
			ppgoin(STAT);
			statement(s);
			ppgoout(STAT);
		}
	}
	if (rescnt(&scnt))
		getcnt();
	if (r[4] != NIL)
		unprint();
	if (s != NIL && s[0] == T_BLOCK)
		ppstbl2();
}

whilop(r)
	int *r;
{
	struct pxcnt scnt;

	ppkw("while");
	ppspac();
	rvalue(r[2], NIL);
	savecnt(&scnt);
	getcnt();
	ppstdo(r[3], STAT);
	if (rescnt(&scnt))
		getcnt();
}

repop(r)
	int *r;
{
	struct pxcnt scnt;

	ppkw("repeat");
	ppgoin(STAT);
	savecnt(&scnt);
	getcnt();
	statlist(r[2]);
	ppgoout(DECL);
	ppnl();
	indent();
	ppkw("until");
	ppspac();
	rvalue(r[3], NIL);
	ppgoin(DECL);
	ppgoout(STAT);
	if (rescnt(&scnt))
		getcnt();
}

ppstbl(r, m)
int *r;
{
	ppstbl1(r, m);
	ppstbl2();
}

ppstbl1(r, m)
int *r;
{
	ppkw("begin");
	ppgoin(m);
	statlist(r[2]);
	ppgoout(m);
}

ppstbl2()
{
	ppnl();
	indent();
	ppkw("end");
}

ppstdo(r, l)
int *r;
{
	register *s;

	ppspac();
	ppkw("do");
	ppspac();
	s = r;
	if (s != NIL && s[0] == T_BLOCK)
		ppstbl(s, l);
	else {
		ppgoin(l);
		statement(s);
		ppgoout(l);
	}
}
