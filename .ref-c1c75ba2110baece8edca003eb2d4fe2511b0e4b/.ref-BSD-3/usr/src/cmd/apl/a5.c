#include "apl.h"

ex_rav()
{
	register struct item *p, *r;

	p = fetch1();
	if(p->rank == 0) {
		r = newdat(p->type, 1, 1);
		putdat(r, getdat(p));
		pop();
		push(r);
		return;
	}
	rav0(p->rank-1);
}

ex_ravk()
{
	register i;

	i = topfix() - thread.iorg;
	fetch1();
	rav0(i);
}

rav0(k)
{
	register struct item *p, *r;
	struct item *param[2];
	int rav1();

	p = sp[-1];
	bidx(p);
	colapse(k);
	r = newdat(p->type, 1, p->size);
	param[0] = p;
	param[1] = r;
	forloop(rav1, param);
	pop();
	push(r);
}

rav1(param)
struct item *param[];
{
	register struct item *p;
	register i, n;

	p = param[0];
	n = access();
	for(i=0; i<idx.dimk; i++) {
		p->index = n;
		putdat(param[1], getdat(p));
		n =+ idx.delk;
	}
}

ex_cat()
{
	register struct item *p, *q;
	struct item *r;
	register k;

	p = fetch2();
	q = sp[-2];
	k = p->rank;
	if(q->rank > k)
		k = q->rank;
	if(k == 0) {
		r = newdat(p->type, 1, 2);
		putdat(r, getdat(p));
		putdat(r, getdat(q));
		pop();
		pop();
		push(r);
	} else
		cat0(k-1);
}

ex_catk()
{
	register k;

	k = topfix() - thread.iorg;
	fetch2();
	cat0(k);
}

cat0(k)
{
	register struct item *p, *q;
	register i;
	struct item *r;
	int a, b;

	p = sp[-1];
	q = sp[-2];
	i = k;
	if(p->rank >=  q->rank) {
		bidx(p);
		b = cat1(q, i);
		a = idx.dim[i];
	} else {
		bidx(q);
		a = cat1(p, i);
		b = idx.dim[i];
	}
	idx.dim[i] = a+b;
	size();
	r = newdat(p->type, idx.rank, idx.size);
	copy(IN, idx.dim, r->dim, idx.rank);
	i = idx.del[i];
	a =* i;
	b =* i;
	while(r->index < r->size) {
		for(i=0; i<a; i++)
			putdat(r, getdat(p));
		for(i=0; i<b; i++)
			putdat(r, getdat(q));
	}
	pop();
	pop();
	push(r);
}

cat1(ip, k)
struct item *ip;
{
	register struct item *p;
	register i, j;
	int a;

	if(k < 0 || k >= idx.rank)
		error("cat X");
	p = ip;
	a = 1;
	if(p->rank == 0)
		return(a);
	j = 0;
	for(i=0; i<idx.rank; i++) {
		if(i == k) {
			if(p->rank == idx.rank) {
				a = p->dim[i];
				j++;
			}
			continue;
		}
		if(idx.dim[i] != p->dim[j])
			error("cat C");
		j++;
	}
	return(a);
}
