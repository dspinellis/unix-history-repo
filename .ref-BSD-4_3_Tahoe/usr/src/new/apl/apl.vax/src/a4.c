static char Sccsid[] = "a4.c @(#)a4.c	1.2	10/5/82 Berkeley ";
#include "apl.h"

/*
 *	parser generates the following for each  label
 *
 *	AUTO-name  CONST  NAME-name  LABEL
 *
 *	(where CONST is the label address)
 */
ex_label()
{
	register struct nlist *n;

	ex_asgn();
	n = (struct nlist *)sp[-1];
	n->itemp->type = LBL;	/* lock out assignments */
	sp--;			/* discard stack */
}


ex_asgn()
{
	register struct nlist *p;
	register struct item *q;

	p = (struct nlist *)sp[-1];
	switch(p->type){
	case QX:
		pop();
		p = nlook("Llx");
		if(p == 0){
			/*
			 * allocate new name:
			 */
			for(p=nlist; p->namep; p++) {}
			p->namep = alloc(4);
			copy(CH, "Llx", p->namep, 4);
			p->type = LV;
			p->use = 0;
			p->itemp = newdat(CH, 0, 0);
		}
		sp++;	/* reset stack */
		break;
	case QD:
		pop();
		ex_print();
		return;
	case QC:
		pop();
		ex_plot();
		return;
	case QQ:
		pop();
		epr0();	/* print w/out '\n'  (in a2.c) */
		return;
	case LV:
		/* The following line checks that it is not the first assignment
		 * to the local variable, in which case itemp has not be set yet
		 * This used to produce an interesting bug when adress 1 was
		 * holding the manifest constant LBL ... just by chance !
		 */
		if (((struct nlist *)p)->itemp != 0) {
			if(((struct nlist *)p)->itemp->type == LBL)
				error("asgn to label");
		}
		break;
	default:
		error("asgn lv");
	}
	if(p->use != 0 && p->use != DA)
		error("asgn var");
	sp--;
	q = fetch1();
	erase(p);
	p->use = DA;
	((struct nlist *)p)->itemp = q;
	sp[-1] = (struct item *)p;
}

ex_elid()
{

	*sp++ = newdat(EL, 0, 0);
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
		if(((struct nlist *)p)->use != DA)
			fetch1(); /* error("used before set"); */
		q = ((struct nlist *)p)->itemp;
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
		if (p->size > 1) {
			if(idx.size != p->size)
				error("assign C");
			f = 1; /* v[i] <- v */
		} else {
			if (idx.size && !p->size)
				error("assign C");
			/* Note -- for idx.size = 0, no assign occurs
			 * anyway, so it is safe to set "datum" to 0
			 */
			datum = p->size ? getdat(p) : 0;
			f = 2; /* v[i] <- s */
		}
		ex_elid();
	} else {
		p = newdat(q->type, idx.rank, idx.size);
		copy(IN, idx.dim, p->dim, idx.rank);
		*sp++ = p;
		f = 0; /* v[i] */
	}
	bidx(q);
	index1(0, f);
	if(f == 0) {
		p = sp[-1];
		sp--;
		for(i=0; i<=n; i++)
			pop();
		*sp++ = p;
	} else {
		pop();		/* pop ELID */
		sp--;		/* skip over LV */
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
		p = ((struct nlist *)sp[-2])->itemp;
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
		if(k < 0 || k >= idx.dim[i])
			error("subscript X");
		idx.idx[i] = k;
		index1(i+1, f);
	}
}
