#include "apl.h"

ex_miot()
{
	register struct item *p;
	register data *dp;
	register i;

	i = topfix();
	if(i < 0)
		error("miot D");
	p = newdat(DA, 1, i);
	dp = p->datap;
	datum = thread.iorg;
	for(; i; i--) {
		*dp++ = datum;
		datum =+ one;
	}
	push(p);
}

ex_mrho()
{
	register struct item *p, *q;
	register data *dp;
	int i;

	p = fetch1();
	q = newdat(DA, 1, p->rank);
	dp = q->datap;
	for(i=0; i<p->rank; i++)
		*dp++ = p->dim[i];
	pop();
	push(q);
}

ex_drho()
{
	register struct item *p, *q;
	struct item *r;
	int s, i;
	register data *dp;
	char *cp;

	p = fetch2();
	q = sp[-2];
	if(p->type != DA || p->rank > 1 || q->size < 1)
		error("rho C");
	s = 1;
	dp = p->datap;
	for(i=0; i<p->size; i++)
		s =* fix(*dp++);
	r = newdat(q->type, p->size, s);
	dp = p->datap;
	for(i=0; i<p->size; i++)
		r->dim[i] = fix(*dp++);
	cp = r->datap;
	while(s > 0) {
		i = s;
		if(i > q->size)
			i = q->size;
		cp =+ copy(q->type, q->datap, cp, i);
		s =- i;
	}
	pop();
	pop();
	push(r);
}
