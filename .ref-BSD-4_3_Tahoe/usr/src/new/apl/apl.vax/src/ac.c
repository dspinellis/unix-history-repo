static char Sccsid[] = "ac.c @(#)ac.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

ex_rot0()
{

	fetch2();
	rotk(0);
}

ex_rotk()
{
	register k;

	k = topfix() - thread.iorg;
	fetch2();
	rotk(k);
}

ex_rot()
{
	register struct item *p;

	fetch2();
	p = sp[-2];
	rotk(p->rank-1);
}

rotk(k)
{
	register struct item *p, *q;
	register param;
	int rot1();

	p = sp[-1];
	bidx(sp[-2]);
	if(k < 0 || k >= idx.rank)
		error("rotate X");
	param = 0;
	colapse(k);
	if(idx.size != p->size) {
		if(p->size != 1)
			error("rotate C");
		param++;
		datum = getdat(p);
	}
	p = newdat(idx.type, 1, idx.dimk);
	*sp++ = p;
	forloop(rot1, param);
	pop();
	pop();
}

rot1(param)
{
	register struct item *p, *q;
	register i;
	int o, n;

	if(param == 0)
		datum = getdat(sp[-2]);
	o = fix(datum);
	if(o < 0)
		o = idx.dimk - (-o % idx.dimk);
	q = sp[-1];
	p = sp[-3];
	q->index = 0;
	n = access();
	for(i=0; i<idx.dimk; i++) {
		p->index = n + (o%idx.dimk)*idx.delk;
		putdat(q, getdat(p));
		o++;
	}
	for(i=0; i<idx.dimk; i++) {
		p->index = n;
		putdat(p, getdat(q));
		n += idx.delk;
	}
}
