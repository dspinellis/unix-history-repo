#include "apl.h"

ex_base()
{
	register struct item *p, *q;
	int s, s1;
	data d, d1;
	double r, b;

	p = fetch2();
	q = sp[-2];
	if(p->rank > 1 || q->rank > 1)
		error("base R");
	b = 1.;
	r = 0.;
	s = p->size;
	s1 = q->size;
	while(s > 0 || s1 > 0) {
		if(s > 0) {
			s--;
			p->index = s;
			d = getdat(p);
		}
		if(s1 > 0) {
			s1--;
			q->index = s1;
			d1 = getdat(q);
		}
		r += d1 * b;
		b *= d;
	}
	pop();
	pop();
	p = newdat(DA, 0, 1);
	push(p);
	d = r;
	putdat(p, d);
}

ex_rep()
{
	register struct item *p, *q;
	register s;
	double a, b, r;

	p = fetch2();
	q = sp[-2];
	if(q->size != 1 || p->rank > 1)
		error("represent R");
	r = getdat(q);
	s = p->size;
	while(s > 0) {
		s--;
		p->index = s;
		b = getdat(p);
		if(b == 0.)
			error("represent D");
		r /= b;
		a = r;
		r = floor(r);
		datum = (a - r) * b;
		p->index = s;
		putdat(p, datum);
	}
	sp--;
	pop();
	push(p);
}
