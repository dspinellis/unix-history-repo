#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/eval2.c,v 1.1 83/01/29 12:48:15 jkf Exp $";
#endif

/*					-[Sat Jan 29 12:34:01 1983 by jkf]-
 * 	eval2.c				$Locker:  $
 * more of the evaluator
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include "global.h"
#include "frame.h"

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
	Savestack(2);
	
	lbot = np;
	protect(fun->ar.accfun);
	for ( ; args != nil ; args = args->d.cdr)  /* stack subscripts */
	  if(evalp) protect(eval(args->d.car));
	  else protect(args->d.car);
	protect(fun);
	vtemp = Lfuncal();
	Restorestack();
	return(vtemp);
}

/* Ifcall :: call foreign function/subroutine
 *   Ifcall is handed a binary object which is the function to call.
 * This function has already been determined to be a foreign function
 * by noticing that its discipline field is a string.  
 * The arguments to pass have already been evaluated and stacked.  We
 * create on the stack a 'callg' type argument list to give to the 
 * function.  What is passed to the foreign function depends on the
 * type of argument.  Certain args are passes directly, others must be
 * copied since the foreign function my want to change them.
 * When the foreign function returns, we may have to box the result,
 * depending on the type of foreign function.
 */
lispval
Ifcall(a)
lispval a;
{
	char *alloca(),*sp();
	long callg_();
	register int *arglist;
	register int index;
	register struct argent *mynp;
	register lispval ltemp;
	int nargs = np - lbot;

	/* put a frame on the stack which will save np and lbot in a
	   easy to find place in a standard way */
	errp = Pushframe(F_TO_FORT,nil,nil);

	arglist = (int *) alloca((nargs + 1) * sizeof(int));
	mynp = lbot;
	*arglist = nargs;
	for(index = 1; index <=  nargs; index++) {
		switch(TYPE(ltemp=mynp->val)) {
			/* fixnums and flonums must be reboxed */
		case INT:
			arglist[index] = (int) sp();
			stack(0);
			*(int *) arglist[index] = ltemp->i;
			break;
		case DOUB:
			stack(0);
			arglist[index] = (int) sp();
			stack(0);
			*(double *) arglist[index] = ltemp->r;
			break;

			/* these cause only part of the structure to be sent */

		case ARRAY:
			arglist[index] = (int) ltemp->ar.data;
			break;


		case BCD:
			arglist[index] = (int) ltemp->bcd.start;
			break;

			/* anything else should be sent directly */

		default:
			arglist[index] = (int) ltemp;
			break;
		}
		mynp++;
	}
	switch(((char *)a->bcd.discipline)[0]) {
		case 'i': /* integer-function */
			ltemp = inewint(callg_(a->bcd.start,arglist));
			break;

		case 'r': /* real-function*/
			{
			double result =
			   (* ((double (*)()) callg_))(a->bcd.start,arglist);
			ltemp = newdoub();
			ltemp->r = result; 
			}
			break;

		case 'f':  /* function */
			ltemp = (lispval) callg_(a->bcd.start,arglist);
			break;

		default:
		case 's': /* subroutine */
			callg_(a->bcd.start,arglist);
			ltemp = tatom;
	}
	errp = Popframe();
	return(ltemp);
}


lispval
ftolsp_(arg1)
lispval arg1;
{
	int count; 
	register lispval *ap = &arg1;
	lispval save;
	Savestack(1);

	if((count = nargs())==0) return;;

	if(errp->class==F_TO_FORT)
		np = errp->svnp;
	errp = Pushframe(F_TO_LISP,nil,nil);
	lbot = np;
	for(; count > 0; count--)
		np++->val = *ap++;
	save = Lfuncal();
	errp = Popframe();
	Restorestack();
	return(save);
}
