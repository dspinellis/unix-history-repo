static char Sccsid[] = "ak.c @(#)ak.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

ex_scn0()
{
	fetch1();
	scan0(0);
}

ex_scan()
{
	register struct item *p;

	p = fetch1();
	scan0(p->rank-1);
}

ex_scnk()
{
	register i;

	i = topfix() - thread.iorg;
	scan0(i);
}

scan0(k)
{
	register struct item *p, *q;
	data *param[2];
	int scan1();

	p = fetch1();
	if(p->type != DA)
		error("scan T");

	bidx(p);
	colapse(k);
	if(idx.dimk == 0) {
/*
 *  scan identities - ets/jrl 5/76
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
	param[0] = p->datap;
	param[1] = (data *)exop[*pcp++];
	forloop(scan1, param);
}

scan1(param)
data *param[];
{
	register i;
	register data *dp;
	data d;
	data (*f)();

	dp = param[0];
	f = (data (*)())param[1];
	dp += access();
	d = *dp;
	for(i = 1; i < idx.dimk; i++) {
		dp += idx.delk;
		*dp = d = (*f)(*dp, d);
	}
}

data scalex = 453.;
data scaley = 453.;
data origx = 0.0;
data origy = 0.0;

ex_plot()
{
	register struct item *p;
	register data *dp;
	register i;
	int ic;
	int x, y;

	p = fetch1();
	if(p->type != DA)
		error("plot T");
	if(p->rank != 2)
		error("plot R");
	if(p->dim[1] != 2)
		error("plot C");

	dp = p->datap;
	if ((i = p->dim[0]) == 0) return;
	ic=0;
	while(i--) {
		x = scalex*(*dp++ - origx);
		y = 454-(scaley*(*dp++ - origy));
		if(x<0 || x >= 576 ||
		 y<0 || y>=454)
			error("plot off screen");
		if(ic)
			line(x,y);
		else {
			move(x,y);
			ic=1;
		}
	}
}
line(x,y)
{
}
move(x,y)
{
}
