static char Sccsid[] = "ae.c @(#)ae.c	1.1	10/1/82 Berkeley ";
#include "apl.h"


char base_com[] = {ADD, MUL};

ex_base()
{
	struct item *extend();
	register struct item *p, *q;
	register i;
	char *savpcp;
	data d1, d2;

	p = fetch2();
	q = sp[-2];
	if(scalar(p)){
		if(q->rank > 0)
			i = q->dim[0];
		else
			i = q->size;
		q = extend(DA, i, p->datap[0]);
		pop();
		*sp++ = p = q;
		q = sp[-2];
	}
	d1 = p->datap[p->size-1];
	p->datap[p->size-1] = 1.0;
	for(i=p->size-2; i>= 0; i--){
		d2 = p->datap[i];
		p->datap[i] = d1;
		d1 *= d2;
	}
	savpcp = pcp;
	pcp = base_com;
	ex_iprod();
	pcp = savpcp;
}

ex_rep()
{
	register struct item *p, *q;
	struct item *r;
	double d1, d2, d3;
	data *p1, *p2, *p3;

	p = fetch2();
	q = sp[-2];
	/*
 	 * first map 1 element vectors to scalars:
	 *
	if(scalar(p))
		p->rank = 0;
	if(scalar(q))
		q->rank = 0;
	 */
	r = newdat(DA,  p->rank+q->rank,  p->size*q->size);
	copy(IN, p->dim, r->dim, p->rank);
	copy(IN, q->dim, r->dim+p->rank, q->rank);
	p3 = &r->datap[r->size];
	for(p1 = &p->datap[p->size]; p1 > p->datap; ){
		d1 = *--p1;
		if(d1 == 0.0)
			d1 = 1.0e38;	/* all else goes here */
		for(p2 = &q->datap[q->size]; p2 > q->datap; ){
			d2 = *--p2;
			d3 = d2 /= d1;
			*p2 = d2 = floor(d2);
			*--p3 = (d3 - d2)*d1;
		}
	}
	pop();
	pop();
	*sp++ = r;
}

/*
 * scalar -- return true if arg is a scalar 
 */
scalar(aip)
struct item *aip;
{
	return(aip->size == 1);
}


struct item *
extend(ty, n, d)
data d;
{
	register i;
	register struct item *q;

	if(ty != DA)
		error("extend T");
	q = newdat(ty, 1, n);
	for(i=0; i<n; i++)
		q->datap[i] = d;
	return(q);
}
