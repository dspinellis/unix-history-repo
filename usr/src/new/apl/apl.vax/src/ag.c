static char Sccsid[] = "ag.c @(#)ag.c	1.2	10/1/82 Berkeley ";
#include "apl.h"

ex_diot()
{
	register struct item *p, *q, *r;
	int i, j;

	p = fetch2();
	q = sp[-2];
	r = newdat(DA, q->rank, q->size);
	copy(IN, q->dim, r->dim, q->rank);
	for(i=0; i<q->size; i++) {
		datum = getdat(q);
		p->index = 0;
		for(j=0; j<p->size; j++)
			if(fuzz(getdat(p), datum) == 0)
				break;
		datum = j + thread.iorg;
		putdat(r, datum);
	}
	pop();
	pop();
	*sp++ = r;
}

ex_eps()
{
	register struct item *p, *q, *r;
	int i, j;
	data d;

	p = fetch2();
	q = sp[-2];
	r = newdat(DA, p->rank, p->size);
	copy(IN, p->dim, r->dim, p->rank);
	for(i=0; i<p->size; i++) {
		datum = getdat(p);
		d = zero;
		q->index = 0;
		for(j=0; j<q->size; j++)
			if(fuzz(getdat(q), datum) == 0) {
				d = one;
				break;
			}
		putdat(r, d);
	}
	pop();
	pop();
	*sp++ = r;
}
