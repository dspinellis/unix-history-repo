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
#include "opcode.h"

extern	char *opnames[];
/*
 * Rvalue - an expression.
 *
 * Contype is the type that the caller would prefer, nand is important
 * if constant sets or constant strings are involved, the latter
 * because of string padding.
 */
rvalue(r, contype)
	int *r;
	struct nl *contype;
{
	register struct nl *p, *p1;
	register struct nl *q;
	int c, c1, *rt, w, g;
	char *cp, *cp1, *opname;
	long l;
	double f;

	if (r == NIL)
		return (NIL);
	if (nowexp(r))
		return (NIL);
	/*
	 * Pick up the name of the operation
	 * for future error messages.
	 */
	if (r[0] <= T_IN)
		opname = opnames[r[0]];

	/*
	 * The root of the tree tells us what sort of expression we have.
	 */
	switch (r[0]) {

	/*
	 * The constant nil
	 */
	case T_NIL:
		put2(O_CON2, 0);
		return (nl+TNIL);

	/*
	 * Function call with arguments.
	 */
	case T_FCALL:
		return (funccod(r));

	case T_VAR:
		p = lookup(r[2]);
		if (p == NIL || p->class == BADUSE)
			return (NIL);
		switch (p->class) {
			case VAR:
				/*
				 * If a variable is
				 * qualified then get
				 * the rvalue by a
				 * lvalue and an ind.
				 */
				if (r[3] != NIL)
					goto ind;
				q = p->type;
				if (q == NIL)
					return (NIL);
				w = width(q);
				switch (w) {
					case 8:
						w = 6;
					case 4:
					case 2:
					case 1:
						put2(O_RV1 + (w >> 1) | bn << 9, p->value[0]);
						break;
					default:
						put3(O_RV | bn << 9, p->value[0], w);
				}
				return (q);

			case WITHPTR:
			case REF:
				/*
				 * A lvalue for these
				 * is actually what one
				 * might consider a rvalue.
				 */
ind:
				q = lvalue(r, NOMOD);
				if (q == NIL)
					return (NIL);
				w = width(q);
				switch (w) {
					case 8:
						w = 6;
					case 4:
					case 2:
					case 1:
						put1(O_IND1 + (w >> 1));
						break;
					default:
						put2(O_IND, w);
				}
				return (q);

			case CONST:
				if (r[3] != NIL) {
					error("%s is a constant and cannot be qualified", r[2]);
					return (NIL);
				}
				q = p->type;
				if (q == NIL)
					return (NIL);
				if (q == nl+TSTR) {
					/*
					 * Find the size of the string
					 * constant if needed.
					 */
					cp = p->value[0];
cstrng:
					cp1 = cp;
					for (c = 0; *cp++; c++)
						continue;
					if (contype != NIL && !opt('s')) {
						if (width(contype) < c && classify(contype) == TSTR) {
							error("Constant string too long");
							return (NIL);
						}
						c = width(contype);
					}
					put3(O_CONG, c, cp1);
					/*
					 * Define the string temporarily
					 * so later people can know its
					 * width.
					 * cleaned out by stat.
					 */
					q = defnl(0, STR, 0, c);
					q->type = q;
					return (q);
				}
				if (q == nl+T1CHAR) {
					put2(O_CONC, p->value[0]);
					return (q);
				}
				/*
				 * Every other kind of constant here
				 */
				switch (width(q)) {
					case 8:
#ifndef DEBUG
						put(5, O_CON8, p->real);
#else
						if (hp21mx) {
							f = p->real;
							conv(&f);
							l = f.plong;
							put3(O_CON4, l);
						} else
							put3(O_CON4, f);
#endif
						break;
					case 4:
						put3(O_CON4, p->range[0]);
						break;
					case 2:
						put2(O_CON2, p->value[1]);
						break;
					case 1:
						put2(O_CON1, p->value[0]);
						break;
					default:
						panic("rval");
					}
				return (q);

			case FUNC:
				/*
				 * Function call with no arguments.
				 */
				if (r[3]) {
					error("Can't qualify a function result value");
					return (NIL);
				}
				return (funccod(r));

			case TYPE:
				error("Type names (e.g. %s) allowed only in declarations", p->symbol);
				return (NIL);

			case PROC:
				error("Procedure %s found where expression required", p->symbol);
				return (NIL);
			default:
				panic("rvid");
		}
	/*
	 * Constant sets
	 */
	case T_CSET:
		return (cset(r, contype, NIL));

	/*
	 * Unary plus and minus
	 */
	case T_PLUS:
	case T_MINUS:
		q = rvalue(r[2], NIL);
		if (q == NIL)
			return (NIL);
		if (isnta(q, "id")) {
			error("Operand of %s must be integer or real, not %s", opname, nameof(q));
			return (NIL);
		}
		if (r[0] == T_MINUS) {
			put1(O_NEG2 + (width(q) >> 2));
			return (isa(q, "d") ? q : nl+T4INT);
		}
		return (q);

	case T_NOT:
		q = rvalue(r[2], NIL);
		if (q == NIL)
			return (NIL);
		if (isnta(q, "b")) {
			error("not must operate on a Boolean, not %s", nameof(q));
			return (NIL);
		}
		put1(O_NOT);
		return (nl+T1BOOL);

	case T_AND:
	case T_OR:
		p = rvalue(r[2], NIL);
		p1 = rvalue(r[3], NIL);
		if (p == NIL || p1 == NIL)
			return (NIL);
		if (isnta(p, "b")) {
			error("Left operand of %s must be Boolean, not %s", opname, nameof(p));
			return (NIL);
		}
		if (isnta(p1, "b")) {
			error("Right operand of %s must be Boolean, not %s", opname, nameof(p1));
			return (NIL);
		}
		put1(r[0] == T_AND ? O_AND : O_OR);
		return (nl+T1BOOL);

	case T_DIVD:
		p = rvalue(r[2], NIL);
		p1 = rvalue(r[3], NIL);
		if (p == NIL || p1 == NIL)
			return (NIL);
		if (isnta(p, "id")) {
			error("Left operand of / must be integer or real, not %s", nameof(p));
			return (NIL);
		}
		if (isnta(p1, "id")) {
			error("Right operand of / must be integer or real, not %s", nameof(p1));
			return (NIL);
		}
		return (gen(NIL, r[0], width(p), width(p1)));

	case T_MULT:
	case T_SUB:
	case T_ADD:
		/*
		 * If the context hasn't told us
		 * the type and a constant set is
		 * present on the left we need to infer
		 * the type from the right if possible
		 * before generating left side code.
		 */
		if (contype == NIL && (rt = r[2]) != NIL && rt[1] == SAWCON) {
			codeoff();
			contype = rvalue(r[3], NIL);
			codeon();
			if (contype == NIL)
				return (NIL);
		}
		p = rvalue(r[2], contype);
		p1 = rvalue(r[3], p);
		if (p == NIL || p1 == NIL)
			return (NIL);
		if (isa(p, "id") && isa(p1, "id"))
			return (gen(NIL, r[0], width(p), width(p1)));
		if (isa(p, "t") && isa(p1, "t")) {
			if (p != p1) {
				error("Set types of operands of %s must be identical", opname);
				return (NIL);
			}
			gen(TSET, r[0], width(p), 0);
			/*
			 * Note that set was filled in by the call
			 * to width above.
			 */
			if (r[0] == T_SUB)
				put2(NIL, 0177777 << ((set.uprbp & 017) + 1));
			return (p);
		}
		if (isnta(p, "idt")) {
			error("Left operand of %s must be integer, real or set, not %s", opname, nameof(p));
			return (NIL);
		}
		if (isnta(p1, "idt")) {
			error("Right operand of %s must be integer, real or set, not %s", opname, nameof(p1));
			return (NIL);
		}
		error("Cannot mix sets with integers and reals as operands of %s", opname);
		return (NIL);

	case T_MOD:
	case T_DIV:
		p = rvalue(r[2], NIL);
		p1 = rvalue(r[3], NIL);
		if (p == NIL || p1 == NIL)
			return (NIL);
		if (isnta(p, "i")) {
			error("Left operand of %s must be integer, not %s", opname, nameof(p));
			return (NIL);
		}
		if (isnta(p1, "i")) {
			error("Right operand of %s must be integer, not %s", opname, nameof(p1));
			return (NIL);
		}
		return (gen(NIL, r[0], width(p), width(p1)));

	case T_EQ:
	case T_NE:
	case T_GE:
	case T_LE:
	case T_GT:
	case T_LT:
		/*
		 * Since there can be no, a priori, knowledge
		 * of the context type should a constant string
		 * or set arise, we must poke around to find such
		 * a type if possible.  Since constant strings can
		 * always masquerade as identifiers, this is always
		 * necessary.
		 */
		codeoff();
		p1 = rvalue(r[3], NIL);
		codeon();
		if (p1 == NIL)
			return (NIL);
		contype = p1;
		if (p1 == nl+TSET || p1->class == STR) {
			/*
			 * For constant strings we want
			 * the longest type so as to be
			 * able to do padding (more importantly
			 * avoiding truncation). For clarity,
			 * we get this length here.
			 */
			codeoff();
			p = rvalue(r[2], NIL);
			codeon();
			if (p == NIL)
				return (NIL);
			if (p1 == nl+TSET || width(p) > width(p1))
				contype = p;
		}
		/*
		 * Now we generate code for
		 * the operands of the relational
		 * operation.
		 */
		p = rvalue(r[2], contype);
		if (p == NIL)
			return (NIL);
		p1 = rvalue(r[3], p);
		if (p1 == NIL)
			return (NIL);
		c = classify(p);
		c1 = classify(p1);
		if (nocomp(c) || nocomp(c1))
			return (NIL);
		g = NIL;
		switch (c) {
			case TBOOL:
			case TCHAR:
				if (c != c1)
					goto clash;
				break;
			case TINT:
			case TDOUBLE:
				if (c1 != TINT && c1 != TDOUBLE)
					goto clash;
				break;
			case TSCAL:
				if (c1 != TSCAL)
					goto clash;
				if (scalar(p) != scalar(p1))
					goto nonident;
				break;
			case TSET:
				if (c1 != TSET)
					goto clash;
				if (p != p1)
					goto nonident;
				g = TSET;
				break;
			case TPTR:
			case TNIL:
				if (c1 != TPTR && c1 != TNIL)
					goto clash;
				if (r[0] != T_EQ && r[0] != T_NE) {
					error("%s not allowed on pointers - only allow = and <>");
					return (NIL);
				}
				break;
			case TSTR:
				if (c1 != TSTR)
					goto clash;
				if (width(p) != width(p1)) {
					error("Strings not same length in %s comparison", opname);
					return (NIL);
				}
				g = TSTR;
				break;
			default:
				panic("rval2");
		}
		return (gen(g, r[0], width(p), width(p1)));
clash:
		error("%ss and %ss cannot be compared - operator was %s", clnames[c], clnames[c1], opname);
		return (NIL);
nonident:
		error("%s types must be identical in comparisons - operator was %s", clnames[c1], opname);
		return (NIL);

	case T_IN:
		rt = r[3];
		if (rt != NIL && rt[0] == T_CSET)
			p1 = cset(rt, NIL, 1);
		else {
			p1 = rvalue(r[3], NIL);
			rt = NIL;
		}
		if (p1 == nl+TSET) {
			warning();
			error("... in [] makes little sense, since it is always false!");
			put1(O_CON1, 0);
			return (nl+T1BOOL);
		}
		p = rvalue(r[2], NIL);
		if (p == NIL || p1 == NIL)
			return (NIL);
		if (p1->class != SET) {
			error("Right operand of 'in' must be a set, not %s", nameof(p1));
			return (NIL);
		}
		if (incompat(p, p1->type, r[2])) {
			cerror("Index type clashed with set component type for 'in'");
			return (NIL);
		}
		convert(p, nl+T2INT);
		setran(p1->type);
		if (rt == NIL)
			put4(O_IN, width(p1), set.lwrb, set.uprbp);
		else
			put1(O_INCT);
		return (nl+T1BOOL);

	default:
		if (r[2] == NIL)
			return (NIL);
		switch (r[0]) {
		default:
			panic("rval3");


		/*
		 * An octal number
		 */
		case T_BINT:
			f = a8tol(r[2]);
			goto conint;
	
		/*
		 * A decimal number
		 */
		case T_INT:
			f = atof(r[2]);
conint:
			if (f > MAXINT || f < MININT) {
				error("Constant too large for this implementation");
				return (NIL);
			}
			l = f;
			if (bytes(l, l) <= 2) {
				put2(O_CON2, c=l);
				return (nl+T2INT);
			}
			put3(O_CON4, l);
			return (nl+T4INT);
	
		/*
		 * A floating point number
		 */
		case T_FINT:
			put(5, O_CON8, atof(r[2]));
			return (nl+TDOUBLE);
	
		/*
		 * Constant strings.  Note that constant characters
		 * are constant strings of length one; there is
		 * no constant string of length one.
		 */
		case T_STRNG:
			cp = r[2];
			if (cp[1] == 0) {
				put2(O_CONC, cp[0]);
				return (nl+T1CHAR);
			}
			goto cstrng;
		}
	
	}
}

/*
 * Can a class appear
 * in a comparison ?
 */
nocomp(c)
	int c;
{

	switch (c) {
		case TFILE:
		case TARY:
		case TREC:
			error("%ss may not participate in comparisons", clnames[c]);
			return (1);
	}
	return (NIL);
}
