/* Copyright (c) 1982, Regents, University of California */

struct sdot	{ long I; struct sdot *CDR; };
struct vl	{ long high; long low; };

struct sdot *mulbig(a,b)
struct sdot *a, *b;
{
	int la = 1, lb = 1;
	long *sa, *sb, *sc, *base, *alloca();
	struct sdot *export();
	register struct sdot *p;
	register int *q, *r, *s;
	long carry = 0, test;
	struct vl work;

	/* compute lengths */
	
	for(p = a; p->CDR; p = p->CDR) la++;
	for(p = b; p->CDR; p = p->CDR) lb++;

	/* allocate storage areas on the stack */

	base = alloca((la + la + lb + lb + 1)*sizeof(long));
	sc = base + la + lb + 1;
	sb = sc + lb;
	sa = sb + la;
	q  = sa;

	/* copy sdots onto stack */
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

	return(export(sc,base));
}
