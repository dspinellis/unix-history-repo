#include "global.h"
lispval
Iarray(fun,args)
register lispval fun,args;
{
	register lispval reg, temp;
	register struct argent *lbot, *np;
	snpand(2);
	
	lbot = np;
	if(np + 3 > nplim)
		namerr();
	np++->val = fun->accfun;
	np++->val = args;
	np++->val = fun;
	return(vtemp = Lfuncal());

}
#define FINTF 1
#define FDOUBF 2
#define FORTSUB 0

lispval
Ifcall(a)
register lispval a;
{
	int *alloca();
	register int *arglist;
	register int index;
	register struct argent *mynp;
	register lispval ltemp;
	register struct argent *lbot;
	register struct argent *np;
	int nargs = np - lbot;

	arglist = alloca((nargs + 1) * sizeof(int));
	mynp = lbot;
	*arglist = nargs;
	for(index = 1; index <=  nargs; index++) {
		switch(TYPE(mynp->val)) {
		case INT:
			arglist[index] = sp();
			stack(0);
			*(int *) arglist[index] = mynp->val->i;
			break;
		case DOUB:
			stack(0);
			arglist[index] = sp();
			stack(0);
			*(double *) arglist[index] = mynp->val->r;
			break;
		case ARRAY:
			arglist[index] = (int) mynp->val->data;
		}
		mynp++;
	}
	switch(a->discipline->i) {
		case FINTF:
			ltemp = inewint(callg(a->entry,arglist));
			break;

		case FDOUBF:
			ltemp = newdoub();
			ltemp->r = (* ((double (*)()) callg))(a->entry,arglist);
			break;

		default:
		case FORTSUB:
			callg(a->entry,arglist);
			ltemp = tatom;
	}
}
callg(funct,arglist)
lispval (*funct)();
int *arglist;
{
	asm("	callg	*8(ap),*4(ap)");
}
