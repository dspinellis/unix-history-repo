#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"
#include "tree.h"
#include "opcode.h"

/*
 * Declare variables of a var part.  DPOFF1 is
 * the local variable storage for all prog/proc/func
 * modules aside from the block mark.  The total size
 * of all the local variables is entered into the
 * size array.
 */
varbeg()
{

	if (parts & VPRT)
		error("All variables must be declared in one var part");
	parts =| VPRT;
	sizes[cbn].om_max = sizes[cbn].om_off = -DPOFF1;
	forechain = NIL;
}

var(vline, vidl, vtype)
	int vline;
	register int *vidl;
	int *vtype;
{
	register struct nl *np;
	register struct om *op;
	long w;
	int o2;

	np = gtype(vtype);
	line = vline;
	w = (lwidth(np) + 1) &~ 1;
	op = &sizes[cbn];
	for (; vidl != NIL; vidl = vidl[2]) {
		op->om_off =- w;
		o2 = op->om_off;
		enter(defnl(vidl[1], VAR, np, o2));
	}
}

varend()
{

	foredecl();
	sizes[cbn].om_max = sizes[cbn].om_off;
}

/*
 * Evening
 */
even(w)
{
	if (w < 0)
		return (w & ~1);
	return ((w+1) & ~1);
}

/*
 * Find the width of a type in bytes.
 */
width(np)
	struct nl *np;
{

	return (lwidth(np));
}

long lwidth(np)
	struct nl *np;
{
	register struct nl *p;
	long w;

	p = np;
	if (p == NIL)
		return (0);
loop:
	switch (p->class) {
		case TYPE:
			switch (nloff(p)) {
				case TNIL:
					return (2);
				case TSTR:
				case TSET:
					panic("width");
				default:
					p = p->type;
					goto loop;
			}
		case ARRAY:
			return (aryconst(p, 0));
		case PTR:
		case FILE:
			return (2);
		case RANGE:
			if (p->type == nl+TDOUBLE)
				return (8);
		case SCAL:
			return (bytes(p->range[0], p->range[1]));
		case SET:
			setran(p->type);
			return ( (set.uprbp>>3) + 1);
		case STR:
		case RECORD:
			w = 0;
			w.pint2 = p->value[NL_OFFS];
			return (w);
		default:
			panic("wclass");
	}
}

/*
 * Return the width of an element
 * of a n time subscripted np.
 */
long aryconst(np, n)
	struct nl *np;
	int n;
{
	register struct nl *p;
	long s, d;

	if ((p = np) == NIL)
		return (NIL);
	if (p->class != ARRAY)
		panic("ary");
	s = width(p->type);
	/*
	 * Arrays of anything but characters are word aligned.
	 */
	if (s & 1)
		if (s != 1)
			s++;
	/*
	 * Skip the first n subscripts
	 */
	while (n >= 0) {
		p = p->chain;
		n--;
	}
	/*
	 * Sum across remaining subscripts.
	 */
	while (p != NIL) {
		if (p->class != RANGE && p->class != SCAL)
			panic("aryran");
		d = p->range[1] - p->range[0] + 1;
		s =* d;
		p = p->chain;
	}
	return (s);
}

/*
 * Find the lower bound of a set, and also its size in bits.
 */
setran(q)
	struct nl *q;
{
	register lb, ub;
	register struct nl *p;

	p = q;
	if (p == NIL)
		return (NIL);
	lb = p->range[0];
	ub = p->range[1];
	if (p->class != RANGE && p->class != SCAL)
		panic("setran");
	set.lwrb = lb;
	/* set.(upperbound prime) = number of bits - 1; */
	set.uprbp = ub-lb;
}

/*
 * Return the number of bytes required to hold an arithmetic quantity
 */
bytes(lb, ub)
	long lb, ub;
{

	if (lb < -32768 || ub > 32767)
		return (4);
	else if (lb < -128 || ub > 127)
		return (2);
	else
		return (1);
}
