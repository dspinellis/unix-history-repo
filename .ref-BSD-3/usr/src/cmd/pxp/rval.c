/* Copyright (c) 1979 Regents of the University of California */
#
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
			ppop(r[0] == T_PLUS ? "+" : "-");
			al = r[2];
			rvalue(r[2], prec(al) > prec(r) || full);
			break;
		case T_NOT:
			ppkw(opname);
			ppspac();
			rvalue(r[2], 1);
			break;
		case T_EQ:
		case T_NE:
		case T_GE:
		case T_LE:
		case T_GT:
		case T_LT:
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
