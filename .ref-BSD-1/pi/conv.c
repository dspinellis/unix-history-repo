#include "whoami"
#ifdef PI
#include "0.h"
#include "opcode.h"

/*
 * Convert a p1 into a p2.
 * Mostly used for different
 * length integers and "to real" conversions.
 */
convert(p1, p2)
	struct nl *p1, *p2;
{
	if (p1 == NIL || p2 == NIL)
		return;
	switch (width(p1) - width(p2)) {
		case -7:
		case -6:
			put1(O_STOD);
			return;
		case -4:
			put1(O_ITOD);
			return;
		case -3:
		case -2:
			put1(O_STOI);
			return;
		case -1:
		case 0:
		case 1:
			return;
		case 2:
		case 3:
			put1(O_ITOS);
			return;
		default:
			panic("convert");
	}
}

/*
 * Compat tells whether
 * p1 and p2 are compatible
 * types for an assignment like
 * context, i.e. value parameters,
 * indicies for 'in', etc.
 */
compat(p1, p2, t)
	struct nl *p1, *p2;
{
	register c1, c2;

	c1 = classify(p1);
	if (c1 == NIL)
		return (NIL);
	c2 = classify(p2);
	if (c2 == NIL)
		return (NIL);
	switch (c1) {
		case TBOOL:
		case TCHAR:
			if (c1 == c2)
				return (1);
			break;
		case TINT:
			if (c2 == TINT)
				return (1);
		case TDOUBLE:
			if (c2 == TDOUBLE)
				return (1);
			if (c2 == TINT && divflg == 0) {
				divchk= 1;
				c1 = classify(rvalue(t, NIL));
				divchk = NIL;
				if (c1 == TINT) {
					error("Type clash: real is incompatible with integer");
					cerror("This resulted because you used '/' which always returns real rather");
					cerror("than 'div' which divides integers and returns integers");
					divflg = 1;
					return (NIL);
				}
			}
			break;
		case TSCAL:
			if (c2 != TSCAL)
				break;
			if (scalar(p1) != scalar(p2)) {
				error("Type clash: non-identical scalar types");
				return (NIL);
			}
			return (1);
		case TSTR:
			if (c2 != TSTR)
				break;
			if (width(p1) != width(p2)) {
				error("Type clash: unequal length strings");
				return (NIL);
			}
			return (1);
		case TNIL:
			if (c2 != TPTR)
				break;
			return (1);
		case TFILE:
			if (c1 != c2)
				break;
			error("Type clash: files not allowed in this context");
			return (NIL);
		default:
			if (c1 != c2)
				break;
			if (p1 != p2) {
				error("Type clash: non-identical %s types", clnames[c1]);
				return (NIL);
			}
			if (p1->nl_flags & NFILES) {
				error("Type clash: %ss with file components not allowed in this context", clnames[c1]);
				return (NIL);
			}
			return (1);
	}
	error("Type clash: %s is incompatible with %s", clnames[c1], clnames[c2]);
	return (NIL);
}

/*
 * Rangechk generates code to
 * check if the type p on top
 * of the stack is in range for
 * assignment to a variable
 * of type q.
 */
rangechk(p, q)
	struct nl *p, *q;
{
	register struct nl *rp;
	register op;
	int wq, wrp;

	if (opt('t') == 0)
		return;
	rp = p;
	if (rp == NIL)
		return;
	if (q == NIL)
		return;
	/*
	 * When op is 1 we are checking length
	 * 4 numbers against length 2 bounds,
	 * and adding it to the opcode forces
	 * generation of appropriate tests.
	 */
	op = 0;
	wq = width(q);
	wrp = width(rp);
	op = wq != wrp && (wq == 4 || wrp == 4);
	if (rp->class == TYPE)
		rp = rp->type;
	switch (rp->class) {
		case RANGE:
			if (rp->range[0] != 0) {
				if (wrp <= 2)
					put3(O_RANG2+op, rp->value[1], rp->value[3]);
				else if (rp != nl+T4INT)
					put(5, O_RANG4+op, rp->range[0], rp->range[1]);
				break;
			}
			/*
			 * Range whose lower bounds are
			 * zero can be treated as scalars.
			 */
		case SCAL:
			if (wrp <= 2)
				put2(O_RSNG2+op, rp->value[3]);
			else
				put3(O_RSNG4+op, rp->range[1]);
			break;
		default:
			panic("rangechk");
	}
}
#endif
