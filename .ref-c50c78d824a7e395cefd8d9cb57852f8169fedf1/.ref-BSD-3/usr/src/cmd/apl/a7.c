#include "apl.h"

ex_iprod()
{
	register i, j;
	struct item *p, *q, *r;
	int param[10], ipr1();

	param[0] = exop[*pcp++];
	param[1] = exop[*pcp++];
	p = fetch2();
	q = sp[-2];
	if(p->type != DA || q->type != DA)
		error("iprod T");
	bidx(p);
	idx.rank--;
	param[2] = idx.dim[idx.rank];
	if(param[2] != q->dim[0])
		error("inner prod C");
	param[3] = q->size/param[2];
	for(i=1; i<q->rank; i++)
		idx.dim[idx.rank++] = q->dim[i];
	r = newdat(DA, idx.rank, size());
	copy(IN, idx.dim, r->dim, idx.rank);
	param[4] = 0;
	param[5] = 0;
	param[6] = p->datap;
	param[7] = q->datap;
	param[8] = r->datap;
	param[9] = p->size;
	forloop(ipr1, param);
	pop();
	pop();
	push(r);
}

ipr1(param)
int param[];
{
	register i, dk;
	int lk, a, b;
	data *dp1, *dp2, *dp3;
	data (*f1)(), (*f2)(), d;

	f1 = param[0];
	f2 = param[1];
	dk = param[2];
	lk = param[3];
	a = param[4];
	b = param[5];
	dp1 = param[6];
	dp2 = param[7];
	dp3 = param[8];
	a =+ dk;
	b =+ (dk * lk);
	for(i=0; i<dk; i++) {
		a--;
		b =- lk;
		d = (*f2)(dp1[a], dp2[b]);
		if(i == 0)
			datum = d; else
			datum = (*f1)(d, datum);
	}
	*dp3++ = datum;
	param[8] = dp3;
	param[5]++;
	if(param[5] >= lk) {
		param[5] = 0;
		param[4] =+ dk;
		if(param[4] >= param[9])
			param[4] = 0;
	}
}

ex_oprod()
{
	register i, j;
	register data *dp;
	struct item *p, *q, *r;
	data *dp1, *dp2;
	data (*f)();

	f = exop[*pcp++];
	p = fetch2();
	q = sp[-2];
	if(p->type != DA || q->type != DA)
		error("oprod T");
	bidx(p);
	for(i=0; i<q->rank; i++)
		idx.dim[idx.rank++] = q->dim[i];
	r = newdat(DA, idx.rank, size());
	copy(IN, idx.dim, r->dim, idx.rank);
	dp = r->datap;
	dp1 = p->datap;
	for(i=0; i<p->size; i++) {
		datum = *dp1++;
		dp2 = q->datap;
		for(j=0; j<q->size; j++)
			*dp++ = (*f)(datum, *dp2++);
	}
	pop();
	pop();
	push(r);
}
