/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 November 1978
 */

#include "whoami"
#include "0.h"
#include "tree.h"
#include "opcode.h"

/*
 * Constant set constructor.
 * settype is the type of the
 * set if we think that we know it
 * if not we try our damndest to figure
 * out what the type should be.
 */
struct nl *
cset(r, settype, x)
	int *r;
	struct nl *settype;
	int x;
{
	register *e;
	register struct nl *t, *exptype;
	int n, *el;

	if (settype == NIL) {
		/*
		 * So far we have no indication
		 * of what the set type should be.
		 * We "look ahead" and try to infer
		 * The type of the constant set
		 * by evaluating one of its members.
		 */
		e = r[2];
		if (e == NIL)
			return (nl+TSET);	/* tenative for [] */
		e = e[1];
		if (e == NIL)
			return (NIL);
		if (e[0] == T_RANG)
			e = e[1];
		codeoff();
		t = rvalue(e, NIL);
		codeon();
		if (t == NIL)
			return (NIL);
		/*
		 * The type of the set, settype, is
		 * deemed to be a set of the base type
		 * of t, which we call exptype.  If,
		 * however, this would involve a
		 * "set of integer", we cop out
		 * and use "intset"'s current scoped
		 * type instead.
		 */
		if (isa(t, "r")) {
			error("Sets may not have 'real' elements");
			return (NIL);
		}
		if (isnta(t, "bcsi")) {
			error("Set elements must be scalars, not %ss", nameof(t));
			return (NIL);
		}
		if (isa(t, "i")) {
			settype = lookup(intset);
			if (settype == NIL)
				panic("intset");
			settype = settype->type;
			if (settype == NIL)
				return (NIL);
			if (isnta(settype, "t")) {
				error("Set default type \"intset\" is not a set");
				return (NIL);
			}
			exptype = settype->type;
		} else {
			exptype = t->type;
			if (exptype == NIL)
				return (NIL);
			if (exptype->class != RANGE)
				exptype = exptype->type;
			settype = defnl(0, SET, exptype, 0);
		}
	} else {
		if (settype->class != SET) {
			/*
			 * e.g string context [1,2] = 'abc'
			 */
			error("Constant set involved in non set context");
			return (NIL);
		}
		exptype = settype->type;
	}
	if (x == NIL)
		put2(O_PUSH, -width(settype));
	n = 0;
	for (el=r[2]; el; el=el[2]) {
		n++;
		e = el[1];
		if (e == NIL)
			return (NIL);
		if (e[0] == T_RANG) {
			t = rvalue(e[2], NIL);
			if (t == NIL) {
				rvalue(e[1], NIL);
				continue;
			}
			if (incompat(t, exptype, e[2]))
				cerror("Upper bound of element type clashed with set type in constant set");
			else
				convert(t, nl+T2INT);
			t = rvalue(e[1], NIL);
			if (t == NIL)
				continue;
			if (incompat(t, exptype, e[1]))
				cerror("Lower bound of element type clashed with set type in constant set");
			else
				convert(t, nl+T2INT);
		} else {
			t = rvalue((int *) e, NLNIL);
			if (t == NIL)
				continue;
			if (incompat(t, exptype, e))
				cerror("Element type clashed with set type in constant set");
			else
				convert(t, nl+T2INT);
			put1(O_SDUP);
		}
	}
	if (x == NIL) {
		setran(exptype);
		put(4, O_CTTOT, n, set.lwrb, set.uprbp);
	} else
		put2(O_CON2, n);
	return (settype);
}
