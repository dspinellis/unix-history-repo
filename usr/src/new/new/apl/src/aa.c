static char Sccsid[] = "aa.c @(#)aa.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

int	gdu();
int	gdd();

ex_gdu()
{
	register struct item *p;

	p = fetch1();
	gd0(p->rank-1, gdu);
}

ex_gduk()
{
	register k;

	k = topfix() - thread.iorg;
	fetch1();
	gd0(k, gdu);
}

ex_gdd()
{
	register struct item *p;

	p = fetch1();
	gd0(p->rank-1, gdd);
}

ex_gddk()
{
	register k;

	k = topfix() - thread.iorg;
	fetch1();
	gd0(k, gdd);
}

gd0(k, f)
int (*f)();
{
	register struct item *p;
	char *param[2];
	int gd1();

	bidx(sp[-1]);
	if(k < 0 || k >= idx.rank)
		error("grade X");
	p = newdat(DA, idx.rank, idx.size);
	copy(IN, idx.dim, p->dim, idx.rank);
	*sp++ = p;
	colapse(k);
	param[0] = alloc(idx.dimk*SINT);
	param[1] = (char *)f;
	forloop(gd1, param);
	free(param[0]);
	p = sp[-1];
	sp--;
	pop();
	*sp++ = p;
}

gd1(param)
int *param[];
{
	register struct item *p;
	register i, *m;

	integ = access();
	m = param[0];
	for(i=0; i<idx.dimk; i++)
		*m++ = i;
	m = param[0];
	qsort(m, idx.dimk, SINT, param[1]);
	p = sp[-1];
	for(i=0; i<idx.dimk; i++) {
		p->index = integ;
		datum = *m++ + thread.iorg;
		putdat(p, datum);
		integ += idx.delk;
	}
}

gdu(p1, p2)
int *p1, *p2;
{
	register struct item *p;
	data d1, d2;

	p = sp[-2];
	p->index = integ + *p1 * idx.delk;
	d1 = getdat(p);
	p->index = integ + *p2 * idx.delk;
	d2 = getdat(p);
	if(fuzz(d1, d2) != 0) {
		if(d1 > d2)
			return(1);
		return(-1);
	}
	return(*p1 - *p2);
}

gdd(p1, p2)
int *p1, *p2;
{
	register struct item *p;
	data d1, d2;

	p = sp[-2];
	p->index = integ + *p1 * idx.delk;
	d1 = getdat(p);
	p->index = integ + *p2 * idx.delk;
	d2 = getdat(p);
	if(fuzz(d1, d2) != 0) {
		if(d1 > d2)
			return(-1);
		return(1);
	}
	return(*p1 - *p2);
}
