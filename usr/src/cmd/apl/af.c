#include "apl.h"

ex_deal()
{
	register struct item *p;
	register m, n;
	double f;
	data d1, d2;

	m = topfix();
	n = topfix();
	if(m < 0 || m > n)
		error("deal D");
	p = newdat(DA, 1, m);
	datum = thread.iorg;
	for(; n!=0; n--) {
		f = m;
		f /= n;
		if(rand()/(32768.*32768.*2.) < f) {
			putdat(p, datum);
			m--;
		}
		datum += one;
	}
	m = p->size;
	while(m > 0) {
		f = rand()/(32768.*32768.*2.);
		n = m * f;
		m--;
		if(n != m) {
			p->index = n;
			d1 = getdat(p);
			p->index = m;
			d2 = getdat(p);
			p->index = n;
			putdat(p, d2);
			p->index = m;
			putdat(p, d1);
		}
	}
	push(p);
}
