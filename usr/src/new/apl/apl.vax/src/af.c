static char Sccsid[] = "af.c @(#)af.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

#ifdef vax
#define	MAXRAND	2147483648.
#else
#define	MAXRAND	32768.
#endif

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
		if(rand()/MAXRAND < f) {
			putdat(p, datum);
			m--;
		}
		datum += one;
	}
	m = p->size;
	while(m > 0) {
		f = rand()/MAXRAND;
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
	*sp++ = p;
}

data
ex_rand(d)
data d;
{
	double f;

	f = (rand()/MAXRAND) * d;
	d = floor(f) + thread.iorg;
	return(d);
}
