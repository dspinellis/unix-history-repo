static char Sccsid[] = "a6.c @(#)a6.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

ex_red0()
{

	fetch1();
	red0(0);
}

ex_red()
{
	register struct item *p;

	p = fetch1();
	red0(p->rank-1);
}

ex_redk()
{
	register i;

	i = topfix() - thread.iorg;
	fetch1();
	red0(i);
}

red0(k)
{
	register struct item *p, *q;
	int param[3], red1();

	p = fetch1();
	if(p->type != DA)
		error("red T");
	bidx(p);
	if (p->rank)
		colapse(k);
	else
		idx.dimk = idx.delk = 1;  /* (handcraft for scalars) */
	if(idx.dimk == 0) {
/*
 *  reduction identities - ets/jrl 5/76
 */
		q = newdat(DA,0,1);
		q->dim[0] = 1;
		switch(*pcp++) {
	case ADD:
	case SUB:
	case OR:
			q->datap[0] = 0;
			break;
	case AND:
	case MUL:
	case DIV:
			q->datap[0] = 1;
			break;
	case MIN:
			q->datap[0] = 1.0e38;
			break;
	case MAX:
			q->datap[0] = -1.0e38;
			break;
	default:
			error("reduce identity");
		}
		pop();
		*sp++ = q;
		return;
	}
	q = newdat(idx.type, idx.rank, idx.size);
	copy(IN, idx.dim, q->dim, idx.rank);
	param[0] = p->datap;
	param[1] = q;
	param[2] = exop[*pcp++];
	forloop(red1, param);
	pop();
	*sp++ = q;
}

red1(param)
int param[];
{
	register i;
	register data *dp;
	data d, (*f)();

	dp = param[0];
	dp += access() + (idx.dimk-1) * idx.delk;
	f = param[2];
	d = *dp;
	for(i=1; i<idx.dimk; i++) {
		dp -= idx.delk;
		d = (*f)(*dp, d);
	}
	putdat(param[1], d);
}
