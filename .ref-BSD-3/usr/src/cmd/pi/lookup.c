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

/*
 * Lookup is called to
 * find a symbol in the
 * block structure symbol
 * table and returns a pointer to
 * its namelist entry.
 */
struct nl *
lookup(s)
	register char *s;
{
	register struct nl *p;
	register struct udinfo *udp;

	if (s == NIL) {
		nocascade();
		return (NIL);
	}
	p = lookup1(s);
	if (p == NIL) {
		derror("%s is undefined", s);
		return (NIL);
	}
	if (p->class == FVAR) {
		p = p->chain;
		bn--;
	}
	return (p);
}

#ifndef PI0
int	flagwas;
#endif
/*
 * Lookup1 is an internal lookup.
 * It is not an error to call lookup1
 * if the symbol is not defined.  Also
 * lookup1 will return FVARs while
 * lookup never will, thus asgnop
 * calls it when it thinks you are
 * assigning to the function variable.
 */

struct nl *
lookup1(s)
	register char *s;
{
	register struct nl *p;
#ifndef PI0
	register struct nl *q;
#endif
	register int i;

	if (s == NIL)
		return (NIL);
	bn = cbn;
#ifndef PI0
	/*
	 * We first check the field names
	 * of the currently active with
	 * statements (expensive since they
	 * are not hashed).
	 */
	for (p = withlist; p != NIL; p = p->nl_next) {
		q = p->type;
		if (q == NIL)
			continue;
		if (reclook(q, s) != NIL)
			/*
			 * Return the WITHPTR, lvalue understands.
			 */
			return (p);
	}
#endif
	/*
	 * Symbol table is a 64 way hash
	 * on the low bits of the character
	 * pointer value. (Simple, but effective)
	 */
	i = (int) s & 077;
	for (p = disptab[i]; p != NIL; p = p->nl_next)
		if (p->symbol == s && p->class != FIELD && p->class != BADUSE) {
			bn = (p->nl_block & 037);
#ifndef PI0
			flagwas = p->nl_flags;
			p->nl_flags |= NUSED;
#endif
			return (p);
		}
	return (NIL);
}

#ifndef PI01
nlfund(sp)
	char *sp;
{
	register struct nl *p;
	register int i;

	i = (int) sp & 077;
	for (p = disptab[i]; p != NIL; p = p->nl_next)
	if (p->symbol == sp && (p->nl_block & 037) == 0)
		return (nloff(p));
	return (0);
}
#endif
