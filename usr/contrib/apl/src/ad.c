static char Sccsid[] = "ad.c @(#)ad.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

ex_com0()
{

	fetch2();
	comk(0);
}

ex_comk()
{
	register k;

	k = topfix() - thread.iorg;
	fetch2();
	comk(k);
}

ex_com()
{
	register struct item *q;

	fetch2();
	q = sp[-2];
	comk(q->rank-1);
}

comk(k)
{
	register struct item *p;
	data d;
	register i;
	int dk, ndk, com1();

	p = sp[-1];
	bidx(sp[-2]);

	/* "getdat" returns the value of the data item which
	 * it is called to fetch.  If this is non-zero, just
	 * use the existing data on the stack (an example in
	 * APL would be "x/y" where x != 0.  If this is zero,
	 * the result is the null item, which is created by
	 * "newdat" and pushed on the stack.
	 */

	if(p->rank == 0 || (p->rank == 1 && p->size == 1)){
		if(getdat(p)) {
			pop();
			return;
		}
		p = newdat(idx.type, 1, 0);
		pop();
		pop();
		*sp++ = p;
		return;
	}

	if(idx.rank == 0 && p->rank == 1) {
		/* then scalar right arg ok */
		dk = p->dim[0];
		ndk = 0;
		for (i=0; i<dk; i++)
			if(getdat(p))
				ndk++;
		p = newdat(idx.type, 1, ndk);
		d = getdat(sp[-2]);
		for(i =0; i<ndk; i++)
			putdat(p,d);
		pop();
		pop();
		*sp++ = p;
		return;
	}
	if(k < 0 || k >= idx.rank)
		error("compress X");
	dk = idx.dim[k];
	if(p->rank != 1 || p->size != dk)
		error("compress C");
	ndk = 0;
	for(i=0; i<dk; i++)
		if(getdat(p))
			ndk++;
	p = newdat(idx.type, idx.rank, (idx.size/dk)*ndk);
	copy(IN, idx.dim, p->dim, idx.rank);
	p->dim[k] = ndk;
	*sp++ = p;
	forloop(com1, k);
	sp--;
	pop();
	pop();
	*sp++ = p;
}

com1(k)
{
	register struct item *p;

	p = sp[-2];
	p->index = idx.idx[k];
	if(getdat(p)) {
		p = sp[-3];
		p->index = access();
		putdat(sp[-1], getdat(p));
	}
}

ex_exd0()
{

	fetch2();
	exdk(0);
}

ex_exdk()
{
	register k;

	k = topfix() - thread.iorg;
	fetch2();
	exdk(k);
}

ex_exd()
{
	register struct item *q;

	fetch2();
	q = sp[-2];
	exdk(q->rank-1);
}

exdk(k)
{
	register struct item *p;
	register i, dk;
	int exd1();

	p = sp[-1];
	bidx(sp[-2]);
	if(k < 0 || k >= idx.rank)
		error("expand X");
	dk = 0;
	for(i=0; i<p->size; i++)
		if(getdat(p))
			dk++;
	if(p->rank != 1 || dk != idx.dim[k])
		error("expand C");
	idx.dim[k] = p->size;
	size();
	p = newdat(idx.type, idx.rank, idx.size);
	copy(IN, idx.dim, p->dim, idx.rank);
	*sp++ = p;
	forloop(exd1, k);
	sp--;
	pop();
	pop();
	*sp++ = p;
}

exd1(k)
{
	register struct item *p;

	p = sp[-2];
	p->index = idx.idx[k];
	if(getdat(p))
		datum = getdat(sp[-3]); else
	if(idx.type == DA)
		datum = zero; else
		datum = ' ';
	putdat(sp[-1], datum);
}
