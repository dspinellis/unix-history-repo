#

/*
 *	monadic epsilon and encode /rww
 */

#include "apl.h"

ex_meps()
{
register struct item *p;
register i,j;
struct item *mark;

	 char *a,*b,*c;
	 int dim0,dim1;
	 int xpcp;

	p = fetch1();
	if(p->rank>2 || p->type!=CH)
		error("execute C");
	if(!p->size) {
		pop();
		push(newdat(DA,1,0));
		return;
	}
	b = p->datap;
	dim0 = p->rank<2 ? 1 : p->dim[0];
	dim1 = p->rank<2 ? p->size : p->dim[1];
	a = alloc(dim1+1);
	xpcp = pcp;
	mark = sp;
	for(i=0; i<dim0; i++){
		for(j=0; j<dim1; j++)
			a[j] = b[j];
		a[j] = '\n';
		c = compile(a,1);
		execute(c);
		afree(c);
		b =+ dim1;
		if(i < dim0-1)
			pop();
	}
	afree(a);
	pcp = xpcp;
	while(sp>mark)
		dealloc(*--sp);
	pop();
	push(newdat(DA,1,0));
}

ex_menc()
{
	struct item *p;

	p = fetch1();
	if(p->type == CH)
		menc0();
	else
		menc1();
}

menc0()			/* dredge up a function and put it into an array*/
{
int	oifile;
	char name[NAMS];
	char *c, *c2;
	struct nlist *np;
	struct item *p;
	int len, dim0, dim1;
	register i;
	register char *dp;

	p = fetch1();
	if(p->size == 0 || p->rank >1 || p->size >= NAMS)
		error("menc C");
			/* set up the name in search format     */
	copy(CH, p->datap, name, p->size);
	name[p->size] = '\0';
			/* search for name among the functions  */
	for(np = nlist; np->namep; np++)
		if(equal(np->namep,name))
			break;
			/* if not found then domain error       */
	if(!np->namep)
		error("menc D");
			/* set up new array                     */
	dim0 = 0;
	dim1 = 0;
	oifile = ifile;
	ifile = dup(wfile);
	lseek(ifile, np->label, 0);    /* look up function     */
			/* compute max width and height         */
	while(c2 = c = rline(0))
	{       while(*c2++ != '\n');
		dim0++;
		len = c2 - c - 1;
		dim1 = dim1 < len ? len : dim1;
		afree(c);
	}
	afree(p);                /* release old variable         */
			/* create new array and put function in */
	p = newdat(CH, 2, dim0*dim1);
	p->rank = 2;
	p->dim[0] = dim0;
	p->dim[1] = dim1;
	dp = p->datap;
	lseek(ifile, np->label, 0);
	while(c2 = c = rline(0))
	{       for(i=0; i<dim1; i++)
			if(*c != '\n')
				*dp++ = *c++;
			else
				*dp++ = ' ';    /* fill w/blanks*/
		afree(c2);
	}
			/* put the new array on the stack       */
	push(p);
			/* reset the current file               */
	ifile = oifile;
}

menc1()/* change numbers into characters       */
{
	struct item *p, *q;
	register i,j,numsz;
	data *dp;
	int total,param[4];

			/* zeroize size information vector      */
	for(i=0; i<4; i++)
		param[i] = 0;
			/* pick up the argument                 */
	p = fetch1();
	dp = p->datap;
			/* find the maximum # of chars in any # */
	for(i=0; i<p->size; i++)
		epr1(*dp++, param);
	numsz = param[1] + param[2] + !!param[2] + param[3] + 1;
			/* rowsize is max # size x last dim     */
	rowsz = p->rank ? p->dim[p->rank-1] : 1;
	rowsz *= numsz;
			/* row size x # of rows(incl blank)*/
	total = p->size * numsz;
	for(j=i=0; i<p->rank; i++)
		if(p->dim[i] != 1)
			if(j++ > 1)
				total =+ rowsz;
			/* make new data and fill with blanks   */
	q = newdat(CH, 2, total);
	q->dim[0] = total/rowsz;
	q->dim[1] = rowsz;
	mencptr = q->datap;
	for(i=0; i<total; i++)
		*mencptr++ = ' ';
	mencptr = q->datap;
			/* use putchar()to fill up the array   */
	mencflg = 2;
	ex_hprint();
	mencflg = 0;
			/* put it on the stack                  */
	push(q);
}
