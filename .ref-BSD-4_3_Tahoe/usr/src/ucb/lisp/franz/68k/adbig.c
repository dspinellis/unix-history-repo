/*
 * $Header: adbig.c,v 1.2 83/11/26 12:12:37 sklower Exp $
 * $Locker:  $
 *
 * Copyright (c) 1982, Regents, University of California
 *
 */
#include "global.h"

struct s_dot	{ long I; struct s_dot *CDR; };
struct vl	{ long high; long low; };

struct s_dot *adbig(a,b)
struct s_dot *a, *b;
{
	int la = 1, lb = 1;
	long *sa, *sb, *sc, *base, *alloca();
	struct s_dot *export();
	register struct s_dot *p;
	register int *q, *r, *s;
	register carry = 0;
	Keepxs();

	/* compute lengths */
	
	for(p = a; p->CDR; p = p->CDR) la++;
	for(p = b; p->CDR; p = p->CDR) lb++;
	if(lb > la) la = lb;

	/* allocate storage areas on the stack */

	base = alloca((3*la+1)*sizeof(long));
	sc = base + la +1;
	sb = sc + la;
	sa = sb + la;
	q  = sa;

	/* copy s_dots onto stack */
	p = a;
	do { *--q = p->I; p = p->CDR; } while (p);
	while(q > sb) *--q = 0;
	p = b;
	do { *--q = p->I; p = p->CDR; } while (p);
	while(q > sc) *--q = 0;

	/* perform the addition */
	for(q = sa, r = sb, s = sc; q > sb;)
	{
		carry += *--q + *--r;
		*--s = carry & 0x3fffffff;
		carry >>= 30;
	}
	*--s = carry;

	p = export(sc,base);
	Freexs();
	return(p);
}
