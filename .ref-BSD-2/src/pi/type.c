/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

/*
 * Type declaration part
 */
typebeg()
{

#ifndef PI1
	if (parts & VPRT)
		error("Type declarations must precede var declarations");
	if (parts & TPRT)
		error("All types must be declared in one type part");
	parts =| TPRT;
#endif
	/*
	 * Forechain is the head of a list of types that
	 * might be self referential.  We chain them up and
	 * process them later.
	 */
	forechain = NIL;
#ifdef PI0
	send(REVTBEG);
#endif
}

type(tline, tid, tdecl)
	int tline;
	char *tid;
	register int *tdecl;
{
	register struct nl *np;

	np = gtype(tdecl);
	line = tline;
	if (np != NIL && (tdecl[0] == T_ID || tdecl[0] == T_TYID))
		np = nlcopy(np);
#ifndef PI0
	enter(defnl(tid, TYPE, np, 0))->nl_flags =| NMOD;
#else
	enter(defnl(tid, TYPE, np, 0));
	send(REVTYPE, tline, tid, tdecl);
#endif
}

typeend()
{

#ifdef PI0
	send(REVTEND);
#endif
	foredecl();
}

/*
 * Return a type pointer (into the namelist)
 * from a parse tree for a type, building
 * namelist entries as needed.
 */
gtype(r)
	register int *r;
{
	register struct nl *np;
	register char *cp;
	int oline;

	if (r == NIL)
		return (NIL);
	oline = line;
	if (r[0] != T_ID)
		oline = line = r[1];
	switch (r[0]) {
		default:
			panic("type");
		case T_TYID:
			r++;
		case T_ID:
			np = lookup(r[1]);
			if (np == NIL)
				break;
			if (np->class != TYPE) {
#ifndef PI1
				error("%s is a %s, not a type as required", r[1], classes[np->class]);
#endif
				np = NIL;
				break;
			}
			np = np->type;
			break;
		case T_TYSCAL:
			np = tyscal(r);
			break;
		case T_TYRANG:
			np = tyrang(r);
			break;
		case T_TYPTR:
			np = defnl(0, PTR, 0, r[2]);
			np->nl_next = forechain;
			forechain = np;
			break;
		case T_TYPACK:
			np = gtype(r[2]);
			break;
		case T_TYARY:
			np = tyary(r);
			break;
		case T_TYREC:
			np = tyrec(r[2], 0);
			break;
		case T_TYFILE:
			np = gtype(r[2]);
			if (np == NIL)
				break;
#ifndef PI1
			if (np->nl_flags & NFILES)
				error("Files cannot be members of files");
#endif
			np = defnl(0, FILE, np, 0);
			np->nl_flags =| NFILES;
			break;
		case T_TYSET:
			np = gtype(r[2]);
			if (np == NIL)
				break;
			if (np->type == nl+TDOUBLE) {
#ifndef PI1
				error("Set of real is not allowed");
#endif
				np = NIL;
				break;
			}
			if (np->class != RANGE && np->class != SCAL) {
#ifndef PI1
				error("Set type must be range or scalar, not %s", nameof(np));
#endif
				np = NIL;
				break;
			}
#ifndef PI1
			if (width(np) > 2)
				error("Implementation restriction: sets must be indexed by 16 bit quantities");
#endif
			np = defnl(0, SET, np, 0);
			break;
	}
	line = oline;
	return (np);
}

/*
 * Scalar (enumerated) types
 */
tyscal(r)
	int *r;
{
	register struct nl *np, *op;
	register *v;
	int i;

	np = defnl(0, SCAL, 0, 0);
	np->type = np;
	v = r[2];
	if (v == NIL)
		return (NIL);
	i = -1;
	for (; v != NIL; v = v[2]) {
		op = enter(defnl(v[1], CONST, np, ++i));
#ifndef PI0
		op->nl_flags =| NMOD;
#endif
		op->value[1] = i;
	}
	np->range[1] = i;
	return (np);
}

/*
 * Declare a subrange.
 */
tyrang(r)
	register int *r;
{
	register struct nl *lp, *hp;
	double high;
	int c, c1;

	gconst(r[3]);
	hp = con.ctype;
	high = con.crval;
	gconst(r[2]);
	lp = con.ctype;
	if (lp == NIL || hp == NIL)
		return (NIL);
	if (norange(lp) || norange(hp))
		return (NIL);
	c = classify(lp);
	c1 = classify(hp);
	if (c != c1) {
#ifndef PI1
		error("Can't mix %ss and %ss in subranges", nameof(lp), nameof(hp));
#endif
		return (NIL);
	}
	if (c == TSCAL && scalar(lp) != scalar(hp)) {
#ifndef PI1
		error("Scalar types must be identical in subranges");
#endif
		return (NIL);
	}
	if (con.crval > high) {
#ifndef PI1
		error("Range lower bound exceeds upper bound");
#endif
		return (NIL);
	}
	lp = defnl(0, RANGE, hp->type, 0);
	lp->range[0] = con.crval;
	lp->range[1] = high;
	return (lp);
}

norange(p)
	register struct nl *p;
{
	if (isa(p, "d")) {
#ifndef PI1
		error("Subrange of real is not allowed");
#endif
		return (1);
	}
	if (isnta(p, "bcsi")) {
#ifndef PI1
		error("Subrange bounds must be Boolean, character, integer or scalar, not %s", nameof(p));
#endif
		return (1);
	}
	return (0);
}

/*
 * Declare arrays and chain together the dimension specification
 */
tyary(r)
	int *r;
{
	struct nl *np;
	register *tl;
	register struct nl *tp, *ltp;
	int i;

	tp = gtype(r[3]);
	if (tp == NIL)
		return (NIL);
	np = defnl(0, ARRAY, tp, 0);
	np->nl_flags =| (tp->nl_flags) & NFILES;
	ltp = np;
	i = 0;
	for (tl = r[2]; tl != NIL; tl = tl[2]) {
		tp = gtype(tl[1]);
		if (tp == NIL) {
			np = NIL;
			continue;
		}
		if (tp->class == RANGE && tp->type == nl+TDOUBLE) {
#ifndef PI1
			error("Index type for arrays cannot be real");
#endif
			np = NIL;
			continue;
		}
		if (tp->class != RANGE && tp->class != SCAL) {
#ifndef PI1
			error("Array index type is a %s, not a range or scalar as required", classes[tp->class]);
#endif
			np = NIL;
			continue;
		}
		if (tp->class == RANGE && bytes(tp->range[0], tp->range[1]) > 2) {
#ifndef PI1
			error("Value of dimension specifier too large or small for this implementation");
#endif
			continue;
		}
		tp = nlcopy(tp);
		i++;
		ltp->chain = tp;
		ltp = tp;
	}
	if (np != NIL)
		np->value[0] = i;
	return (np);
}

/*
 * Delayed processing for pointers to
 * allow self-referential and mutually
 * recursive pointer constructs.
 */
foredecl()
{
	register struct nl *p, *q;

	for (p = forechain; p != NIL; p = p->nl_next) {
		if (p->class == PTR && p->value[0] != 0)
		{
			p->type = gtype(p->value[0]);
#ifndef PI1
			if (p->type != NIL && (p->type->nl_flags & NFILES))
				error("Files cannot be members of dynamic structures");
#endif
			p->value[0] = 0;
		}
	}
}
