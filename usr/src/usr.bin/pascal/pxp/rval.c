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
static char sccsid[] = "@(#)rval.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

extern	char *opnames[];

#define alph(c)		((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
/*
 * Rvalue reformats an expression.
 * Par is a flag indicating that the expression
 * should be parenthesized if it is non-atomic.
 */
rvalue(r, par)
	register int *r;
	int par;
{
	register int *al;
	register char *opname;

	if (r == NIL) {
		ppid("{expr}");
		return;
	}
	if (r[0] <= T_IN)
		opname = opnames[r[0]];
	switch (r[0]) {
		case T_BINT:
		case T_INT:
		case T_FINT:
			ppnumb(r[2]);
			if (r[0] == T_BINT)
				ppsep("b");
			return;
		case T_NIL:
			ppkw("nil");
			return;
		case T_FCALL:
			funccod(r);
			return;
		case T_VAR:
			lvalue(r);
			return;
		case T_CSET:
			cset(r);
			return;
		case T_STRNG:
			ppstr(r[2]);
			return;
	}
	if (par)
		ppbra("(");
	switch (r[0]) {
		default:
			panic("rval");
		case T_PLUS:
		case T_MINUS:
			/*
			 * if child is relational (bogus) or adding operator,
			 * parenthesize child.
			 * this has the unaesthetic property that
			 * --i prints as -(-i), but is needed to catch
			 * -(a+b) which must print as -(a+b), not as -a+b.
			 * otherwise child has higher precedence
			 * and need not be parenthesized.
			 */
			ppop(r[0] == T_PLUS ? "+" : "-");
			al = r[2];
			rvalue(r[2], prec(al) <= prec(r) || full);
			break;
		case T_NOT:
			/*
			 * if child is of lesser precedence
			 * (i.e. not another not operator)
			 * parenthesize it.
			 * nested not operators need not be parenthesized
			 * because it's a prefix operator.
			 */
			ppkw(opname);
			ppspac();
			al = r[2];
			rvalue(r[2], prec(al) < prec(r) || full);
			break;
		case T_EQ:
		case T_NE:
		case T_GE:
		case T_LE:
		case T_GT:
		case T_LT:
			/*
			 * make the aesthetic choice to
			 * fully parenthesize relational expressions,
			 * in spite of left to right associativity.
			 * note: there are no operators with lower precedence.
			 */
			al = r[2];
			rvalue(al, prec(al) <= prec(r) || full);
			goto rest;
		case T_AND:
		case T_OR:
		case T_MULT:
		case T_ADD:
		case T_SUB:
		case T_DIVD:
		case T_MOD:
		case T_DIV:
		case T_IN:
			/*
			 * need not parenthesize left child
			 * if it has equal precedence,
			 * due to left to right associativity.
			 * right child needs to be parenthesized
			 * if it has equal (or lesser) precedence.
			 */
			al = r[2];
			rvalue(al, prec(al) < prec(r) || full);
rest:
			ppspac();
			if (alph(opname[0]))
				ppkw(opname);
			else
				ppop(opname);
			ppspac();
			al = r[3];
			rvalue(al, prec(al) <= prec(r) || full);
			break;
	}
	if (par)
		ppket(")");
}

/*
 * Prec returns the precedence of an operator,
 * with larger numbers indicating stronger binding.
 * This is used to determine when parenthesization
 * is needed on subexpressions.
 */
prec(r)
	register int *r;
{

	if (r == NIL)
		return;
	switch (r[0]) {
		case T_NOT:
			return (3);
		case T_MULT:
		case T_DIVD:
		case T_DIV:
		case T_MOD:
		case T_AND:
			return (2);
		case T_ADD:
		case T_SUB:
		case T_OR:
		case T_PLUS:
		case T_MINUS:
			return (1);
		default:
			return (0);
	}
}
