/* Copyright (c) 1982, Regents, University of California */
struct sdot	{ long I; struct sdot *CDR; };
struct vl	{ long high; long low; };

struct sdot *adbig(a,b)
struct sdot *a, *b;
{
	int la = 1, lb = 1;
	long *sa, *sb, *sc, *base, *alloca();
	struct sdot *export();
	register struct sdot *p;
	register int *q, *r, *s;
	register carry = 0;

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

	/* copy sdots onto stack */
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

	return(export(sc,base));
}
