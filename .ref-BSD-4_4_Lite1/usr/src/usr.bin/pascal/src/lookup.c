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
static char sccsid[] = "@(#)lookup.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "0.h"

struct nl *disptab[077+1];

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
	register struct udinfo;

	if (s == NIL) {
		nocascade();
		return (NLNIL);
	}
	p = lookup1(s);
	if (p == NLNIL) {
		derror("%s is undefined", s);
		return (NLNIL);
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
		return (NLNIL);
	bn = cbn;
#ifndef PI0
	/*
	 * We first check the field names
	 * of the currently active with
	 * statements (expensive since they
	 * are not hashed).
	 */
	for (p = withlist; p != NLNIL; p = p->nl_next) {
		q = p->type;
		if (q == NLNIL)
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
	for (p = disptab[i]; p != NLNIL; p = p->nl_next)
		if (p->symbol == s && p->class != FIELD && p->class != BADUSE) {
			bn = (p->nl_block & 037);
#ifndef PI0
			flagwas = p->nl_flags;
			p->nl_flags |= NUSED;
#endif
			return (p);
		}
	return (NLNIL);
}

#ifndef PI01
nlfund(sp)
	char *sp;
{
	register struct nl *p;
	register int i;

	i = (int) sp & 077;
	for (p = disptab[i]; p != NLNIL; p = p->nl_next)
	if (p->symbol == sp && (p->nl_block & 037) == 0)
		return (nloff(p));
	return (0);
}
#endif
