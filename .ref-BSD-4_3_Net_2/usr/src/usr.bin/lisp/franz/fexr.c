#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/fexr.c,v 1.1 83/01/29 12:48:43 jkf Exp $";
#endif

/*					-[Sat Jan 29 12:41:19 1983 by jkf]-
 * 	fexr.c				$Locker:  $
 * nlambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */


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

	if( TYPE(lbot->val) != DTPR )
		return(error("BAD CALL TO OPVAL",TRUE));
	quant = eval(lbot->val->d.car);	/*  evaluate name of sys variable  */
	while( TYPE(quant) != ATOM )
		quant = error("FIRST ARG TO OPVAL MUST BE AN ATOM",TRUE);

	if( (vtemp=lbot->val->d.cdr) != nil && TYPE(lbot->val->d.cdr) != DTPR )
		return(error("BAD ARG LIST FOR OPVAL",TRUE));
	return(copval(
		quant,
		vtemp==nil ? (lispval)CNIL : eval(vtemp->d.car)
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

	if( option->a.plist == nil && value != (lispval) CNIL)
		{
		protect(option); protect(value);
		option->a.plist = newdot();
		option->a.plist->d.car = sysa;
		option->a.plist->d.cdr = newdot();
		option->a.plist->d.cdr->d.car = value;
		unprot(); unprot();
		return(nil);
		}


	if( option->a.plist == nil ) return(nil);

	fake.cdr = option->a.plist;
	option = (lispval) (&fake);

	while( option->d.cdr != nil )	/*  can't be nil first time through  */
		{
		option = option->d.cdr;
		if( option->d.car == sysa )
			{
			rval = option->d.cdr->d.car;
			if( value != (lispval)CNIL )
				option->d.cdr->d.car = value;
			return(rval);
			}
		option = option->d.cdr;
		}

	if( value != (lispval)CNIL )
		{
		protect(option); protect(value);
		option->d.cdr = newdot();
		option->d.cdr->d.car = sysa;
		option->d.cdr->d.cdr = newdot();
		option->d.cdr->d.cdr->d.car = value;
		unprot(); unprot();
		}


	return(nil);
	}
