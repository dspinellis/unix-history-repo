/*
 * $Header: mulbig.c,v 1.2 83/11/26 12:13:29 sklower Exp $
 * $Locker:  $
 *
 * Copyright (c) 1982, Regents, University of California
 *
 */

#include "global.h"

struct s_dot	{ long I; struct s_dot *CDR; };
struct vl	{ long high; long low; };

struct s_dot *mulbig(a,b)
struct s_dot *a, *b;
{
	int la = 1, lb = 1;
	long *sa, *sb, *sc, *base, *alloca();
	struct s_dot *export();
	register struct s_dot *p;
	register int *q, *r, *s;
	long carry = 0, test;
	struct vl work;
	Keepxs();

	/* compute lengths */
	
	for(p = a; p->CDR; p = p->CDR) la++;
	for(p = b; p->CDR; p = p->CDR) lb++;

	/* allocate storage areas on the stack */

	base = alloca((la + la + lb + lb + 1)*sizeof(long));
	sc = base + la + lb + 1;
	sb = sc + lb;
	sa = sb + la;
	q  = sa;

	/* copy s_dots onto stack */
	p = a;
	do { *--q = p->I; p = p->CDR; } while (p);
	p = b;
	do { *--q = p->I; p = p->CDR; } while (p);
	while(q > base) *--q = 0;  /* initialize target */

	/* perform the multiplication */
	for(q = sb; q > sc; *--s = carry)
	    for((r = sa, s = (q--) - lb, carry = 0); r > sb;)
	    {
		    carry += *--s;
		    emul(*q,*--r,carry,&work);
		    test = work.low;
		    carry = work.high << 2;
		    if(test < 0) carry += 2;
		    if(test & 0x40000000) carry +=1;
		    *s = test & 0x3fffffff;
	    }

	p = export(sc,base);
	Freexs();
	return(p);
}
