static char Sccsid[] = "a7.c @(#)a7.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

ex_iprod()
{
	register i, j;
	struct item *p, *q, *r;
	int param[10], ipr1();
	data (*fn)();

	param[0] = exop[*pcp++];
	param[1] = exop[*pcp++];
	p = fetch2();
	q = sp[-2];
	if(p->type != DA || q->type != DA)
		error("iprod T");
	/*
	 * extend scalars to match corresponding arg
	 */
	if(scalar(p)) {
		if(scalar(q)){
			r = newdat(DA, 0, 1);
			fn = param[1];
			r->datap[0] = (*fn)(p->datap[0], q->datap[0]);
			goto out;
		}
		r = extend(DA, q->dim[0], p->datap[0]);
		pop();
		*sp++ = p = r;
	}
	if(scalar(q)){
		r = extend(DA,p->dim[p->rank - 1], q->datap[0]);
		free(sp[-2]);
		sp[-2] = q = r;
	}
	bidx(p);
	idx.rank--;
	param[2] = idx.dim[idx.rank];
	if((param[2] != q->dim[0]))
/*	&& (param[2] != 1)	*/
/*	&& (q->dim[0] != 1)	*/
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
out:
	pop();
	pop();
	/*
	 * KLUDGE (we need the dim[0]'s for above stuff to work)
	 */
	if(r->rank == 1 && r->size == 1)
		r->rank = 0;
	*sp++ = r;
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
	a += dk;
	b += (dk * lk);
	for(i=0; i<dk; i++) {
		a--;
		b -= lk;
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
		param[4] += dk;
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

	f = (data *)exop[*pcp++];
	p = fetch2();
	q = sp[-2];
	if(p->type != DA || q->type != DA)
		error("oprod T");
	/*
	 * collapse 1 element vectors to scalars
	 *
	if(scalar(p))
		p->rank = 0;
	if(scalar(q))
		q->rank = 0;
	*/
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
	*sp++ = r;
}
