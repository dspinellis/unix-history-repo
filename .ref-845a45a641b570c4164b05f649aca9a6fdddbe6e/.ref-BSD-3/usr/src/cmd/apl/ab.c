#include "apl.h"

ex_take()
{
	register i, k, o;

	o = 0;
	td1();
	for(i=0; i<idx.rank; i++) {
		k = idx.idx[i];
		if(k < 0) {
			k = -k;
			o =+ idx.del[i] *
				(idx.dim[i] - k);
		}
		idx.dim[i] = k;
	}
	map(o);
}

ex_drop()
{
	register i, k, o;

	o = 0;
	td1();
	for(i=0; i<idx.rank; i++) {
		k = idx.idx[i];
		if(k > 0)
			o =+ idx.del[i] * k; else
			k = -k;
		idx.dim[i] =- k;
	}
	map(o);
}

td1()
{
	register struct item *p;
	struct item *q;
	register i, k;

	p = fetch2();
	q = sp[-2];
	if(p->rank > 1 || q->rank !=  p->size)
		error("take C");
	bidx(q);
	for(i=0; i<p->size; i++) {
		k = fix(getdat(p));
		idx.idx[i] = k;
		if(k < 0)
			k = -k;
		if(k > idx.dim[i])
			error("take C");
	}
	pop();
}

ex_dtrn()
{
	register struct item *p, *q;
	register i;

	p = fetch2();
	q = sp[-2];
	if(p->rank > 1 || p->size != q->rank)
		error("tranpose C");
	for(i=0; i<p->size; i++)
		idx.idx[i] = fix(getdat(p)) - thread.iorg;
	pop();
	trn0();
}

ex_mtrn()
{
	register struct item *p;
	register i;

	p = fetch1();
	if(p->rank <= 1)
		return;
	for(i=0; i<p->rank; i++)
		idx.idx[i] = i;
	idx.idx[i-1] = i-2;
	idx.idx[i-2] = i-1;
	trn0();
}

trn0()
{
	register i, j;
	int d[MRANK], r[MRANK];

	bidx(sp[-1]);
	for(i=0; i<idx.rank; i++)
		d[i] = -1;
	for(i=0; i<idx.rank; i++) {
		j = idx.idx[i];
		if(j<0 || j>=idx.rank)
			error("tranpose X");
		if(d[j] != -1) {
			if(idx.dim[i] < d[j])
				d[j] = idx.dim[i];
			r[j] =+ idx.del[i];
		} else {
			d[j] = idx.dim[i];
			r[j] = idx.del[i];
		}
	}
	j = idx.rank;
	for(i=0; i<idx.rank; i++) {
		if(d[i] != -1) {
			if(i > j)
				error("tranpose D");
			idx.dim[i] = d[i];
			idx.del[i] = r[i];
		} else
		if(i < j)
			j = i;
	}
	idx.rank = j;
	map(0);
}

ex_rev0()
{

	fetch1();
	revk(0);
}

ex_revk()
{
	register k;

	k = topfix() - thread.iorg;
	fetch1();
	revk(k);
}

ex_rev()
{
	register struct item *p;

	p = fetch1();
	revk(p->rank-1);
}

revk(k)
{
	register o;

	bidx(sp[-1]);
	if(k < 0 || k >= idx.rank)
		error("reverse X");
	o = idx.del[k] * (idx.dim[k]-1);
	idx.del[k] = -idx.del[k];
	map(o);
}

map(o)
{
	register struct item *p;
	register n, i;
	int map1();

	n = 1;
	for(i=0; i<idx.rank; i++)
		n =* idx.dim[i];
	p = newdat(idx.type, idx.rank, n);
	copy(IN, idx.dim, p->dim, idx.rank);
	push(p);
	forloop(map1, o);
	sp--;
	pop();
	push(p);
}

map1(o)
{
	register struct item *p;

	p = sp[-2];
	p->index = access() + o;
	putdat(sp[-1], getdat(p));
}
