static char *sccsid = "@(#)eval2.c	34.1 10/3/80";

#include "global.h"

/* Iarray - handle array call.
 *  fun - array object
 *  args - arguments to the array call , most likely subscripts.
 *  evalp - flag, if TRUE then the arguments should be evaluated when they
 *	are stacked.
 */
lispval
Iarray(fun,args,evalp)
register lispval fun,args;
{
	register lispval reg, temp;
	register struct argent *lbot, *np;
	
	lbot = np;
	protect(fun->ar.accfun);
	for ( ; args != nil ; args = args->d.cdr)  /* stack subscripts */
	  if(evalp) protect(eval(args->d.car));
	  else protect(args->d.car);
	protect(fun);
	return(vtemp = Lfuncal());
}

lispval
Ifcall(a)
lispval a;
{
	int *alloca();
	register int *arglist;
	register int index;
	register struct argent *mynp;
	register lispval ltemp;
	register struct argent *lbot;
	register struct argent *np;
	int itemp;
	int nargs = np - lbot;

	arglist = alloca((nargs + 1) * sizeof(int));
	mynp = lbot;
	*arglist = nargs;
	for(index = 1; index <=  nargs; index++) {
		switch(TYPE(ltemp=mynp->val)) {
		case INT:
			arglist[index] = sp();
			stack(0);
			*(int *) arglist[index] = ltemp->i;
			break;
		case DOUB:
			stack(0);
			arglist[index] = sp();
			stack(0);
			*(double *) arglist[index] = ltemp->r;
			break;
		case HUNK2:
		case HUNK4:
		case HUNK8:
		case HUNK16:
		case HUNK32:
		case HUNK64:
		case HUNK128:
		case DTPR:
		case ATOM:
		case SDOT:
			arglist[index] = (int) ltemp;
			break;

		case ARRAY:
			arglist[index] = (int) ltemp->ar.data;
			break;


		case BCD:
			arglist[index] = (int) ltemp->bcd.entry;
			break;

		default:
			error("foreign call: illegal argument ",FALSE);
			break;
		}
		mynp++;
	}
	switch(((char *)a->bcd.discipline)[0]) {
		case 'i': /* integer-function */
			ltemp = inewint(callg(a->bcd.entry,arglist));
			break;

		case 'r': /* real-function*/
			ltemp = newdoub();
			ltemp->r = (* ((double (*)()) callg))(a->bcd.entry,arglist);
			break;

		case 'f':  /* function */
			ltemp = (lispval) callg(a->bcd.entry,arglist);
			break;

		default:
		case 's': /* subroutine */
			callg(a->bcd.entry,arglist);
			ltemp = tatom;
	}
	return(ltemp);
}
callg(funct,arglist)
lispval (*funct)();
int *arglist;
{
	asm("	callg	*8(ap),*4(ap)");
}
