#include "apl.h"

ex_asgn()
{
	register struct nlist *p;
	register struct item *q;

	p = sp[-1];
	if(p->type == QD) {
		pop();
		ex_print();
		return;
	}
	if(p->type == QC) {
		pop();
		ex_plot();
		return;
	}
	if(p->type != LV)
		error("asgn lv");
	if(p->use != 0 && p->use != DA)
		error("asgn var");
	sp--;
	q = fetch1();
	erase(p);
	p->use = DA;
	p->itemp = q;
	sp[-1] = p;
}

ex_elid()
{

	push(newdat(EL,0,0));
}

ex_index()
{
	register struct item *p;
	struct item *q;
	register i, j;
	int f, n, lv;

	n = *pcp++;
	f = *pcp;
	p = sp[-1];
	if(f == ASGN) {
		pcp++;
		if(p->type != LV)
			error("indexed assign value");
		if(p->use != DA)
			fetch1(); /* error("used before set"); */
		q = p->itemp;
	} else
		q = fetch1();
	if(q->rank != n)
		error("subscript C");
	idx.rank = 0;
	for(i=0; i<n; i++) {
		p = sp[-i-2];
		if(p->type == EL) {
			idx.dim[idx.rank++] =
				q->dim[i];
			continue;
		}
		p = fetch(p);
		sp[-i-2] = p;
		for(j=0; j<p->rank; j++)
			idx.dim[idx.rank++] =
				p->dim[j];
	}
	size();
	if(f == ASGN) {
		p = fetch(sp[-n-2]);
		sp[-n-2] = p;
		if(p->size > 1) {
			if(idx.size != p->size)
				error("assign C");
			f = 1; /* v[i] <- v */
		} else {
			datum = getdat(p);
			f = 2; /* v[i] <- s */
		}
		ex_elid();
	} else {
		p = newdat(q->type, idx.rank, idx.size);
		copy(IN, idx.dim, p->dim, idx.rank);
		push(p);
		f = 0; /* v[i] */
	}
	bidx(q);
	index1(0, f);
	if(f == 0) {
		p = sp[-1];
		sp--;
		for(i=0; i<=n; i++)
			pop();
		push(p);
	} else {
		sp -= 2;
		for(i=0; i<n; i++)
			pop();
	}
}

index1(i, f)
{
	register struct item *p;
	register j, k;

	if(i >= idx.rank)
	switch(f) {

	case 0:
		p = sp[-2];
		p->index = access();
		putdat(sp[-1], getdat(p));
		return;

	case 1:
		datum = getdat(sp[-idx.rank-3]);

	case 2:
		p = sp[-2]->itemp;
		p->index = access();
		putdat(p, datum);
		return;
	}
	p = sp[-i-3];
	if(p->type == EL) {
		for(j=0; j<idx.dim[i]; j++) {
			idx.idx[i] = j;
			index1(i+1, f);
		}
		return;
	}
	p->index = 0;
	for(j=0; j<p->size; j++) {
		k = fix(getdat(p)) - thread.iorg;
		if(k < 0 || k > idx.dim[i])
			error("subscript X");
		idx.idx[i] = k;
		index1(i+1, f);
	}
}
