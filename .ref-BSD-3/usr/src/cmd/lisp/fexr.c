#include "global.h"

/* Ngcafter *************************************************************/
/*									*/
/*  Default garbage collector routine which does nothing.		*/

lispval 
Ngcafter()
	{
	return(nil);
	}

/*  Nopval  *************************************************************/
/*									*/
/*  Routine which allows system registers and options to be examined	*/
/*  and modified.  Calls copval, the routine which is called by c code	*/
/*  to do the same thing from inside the system.			*/

lispval 
Nopval()
	{
	lispval quant;
	snpand(0);

	if( TYPE(lbot->val) != DTPR )
		return(error("BAD CALL TO OPVAL",TRUE));
	quant = eval(lbot->val->car);	/*  evaluate name of sys variable  */
	while( TYPE(quant) != ATOM )
		quant = error("FIRST ARG TO OPVAL MUST BE AN ATOM",TRUE);

	if( (vtemp=lbot->val->cdr) != nil && TYPE(lbot->val->cdr) != DTPR )
		return(error("BAD ARG LIST FOR OPVAL",TRUE));
	return(copval(
		quant,
		vtemp==nil ? (lispval)CNIL : eval(vtemp->car)
		));
	}
/*  copval  *************************************************************/
/*  This routine keeps track of system quantities, and is called from	*/
/*  C code.  If the second argument is CNIL, no change is made in the	*/
/*  quantity.								*/
/*  Since this routine may call newdot() if the second argument is not	*/
/*  CNIL, the arguments should be protected somehow in that case.	*/

lispval 
copval(option,value)
	lispval option, value;
	{
	struct dtpr fake;
	lispval rval;
	snpand(0);


	if( option->plist == nil && value != (lispval) CNIL)
		{
		protect(option); protect(value);
		option->plist = newdot();
		option->plist->car = sysa;
		option->plist->cdr = newdot();
		option->plist->cdr->car = value;
		unprot(); unprot();
		return(nil);
		}


	if( option->plist == nil ) return(nil);

	fake.cdr = option->plist;
	option = (lispval) (&fake);

	while( option->cdr != nil )	/*  can't be nil first time through  */
		{
		option = option->cdr;
		if( option->car == sysa )
			{
			rval = option->cdr->car;
			if( value != (lispval)CNIL )
				option->cdr->car = value;
			return(rval);
			}
		option = option->cdr;
		}

	if( value != (lispval)CNIL )
		{
		protect(option); protect(value);
		option->cdr = newdot();
		option->cdr->car = sysa;
		option->cdr->cdr = newdot();
		option->cdr->cdr->car = value;
		unprot(); unprot();
		}


	return(nil);
	}
