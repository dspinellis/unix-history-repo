#ifndef lint
static char *rcsid =
   "$Header: fex1.c,v 1.5 85/03/24 11:03:51 sklower Exp $";
#endif

/*					-[Sat Mar  5 19:50:28 1983 by layer]-
 * 	fex1.c				$Locker:  $
 * nlambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include "global.h"
#include "frame.h"

/* Nprog ****************************************************************/
/* This first sets the local variables to nil while saving their old	*/
/* values on the name stack.  Then, pointers to various things are	*/
/* saved as this function may be returned to by an "Ngo" or by a	*/
/* "Lreturn".  At the end is the loop that cycles through the contents	*/
/* of the prog.								*/

lispval
Nprog() {
	register lispval where, temp;
	struct nament *savedbnp = bnp;
	extern struct frame *errp;
	pbuf pb;
	extern int retval;
	extern lispval lispretval;

	if((np-lbot) < 1) chkarg(1,"prog");

	/* shallow bind the local variables to nil */
	if(lbot->val->d.car != nil)
	{
	    for( where = lbot->val->d.car ; where != nil; where = where->d.cdr )
	    {
	        if(TYPE(where) != DTPR || TYPE(temp=where->d.car) != ATOM)
		    errorh1(Vermisc,
			   "Illegal local variable list in prog ",nil,FALSE,
			   1,where);
    	        PUSHDOWN(temp,nil);
	    }
	}

	/* put a frame on the stack which can be 'return'ed to or 'go'ed to */
	errp = Pushframe(F_PROG,nil,nil);

	where = lbot->val->d.cdr;	/* first thing in the prog body */

	switch (retval)	{
	case C_RET:	/*
			 * returning from this prog, value to return
			 * is in lispretval
			 */
			errp = Popframe();
			popnames(savedbnp);
			return(lispretval);

	case C_GO:	/*
			 * going to a certain label, label to go to in
			 * in lispretval
			 */
			where = (lbot->val)->d.cdr;
			while ((TYPE(where) == DTPR) 
			       && (where->d.car != lispretval))
				where = where->d.cdr;
			if (where->d.car == lispretval) {
				popnames(errp->svbnp);
				break;
			}
			/* label not found in this prog, must 
			 * go up to higher prog
			 */
			errp = Popframe();	/* go to next frame */
			Inonlocalgo(C_GO,lispretval,nil);

			/* NOT REACHED */

	case C_INITIAL: break;

	}

	while (TYPE(where) == DTPR)
		{
		temp = where->d.car;
		if((TYPE(temp))!=ATOM) eval(temp);
		where = where->d.cdr;
		}
	if((where != nil) && (TYPE(where) != DTPR)) 
	    errorh1(Vermisc,"Illegal form in prog body ", nil,FALSE,0,where);
	errp = Popframe();
	popnames(savedbnp);	/* pop off locals */
	return(nil);
}

lispval globtag;
/*
   Ncatch is now linked to the lisp symbol *catch , which has the form
     (*catch tag form)
    tag is evaluated and then the catch entry is set up.
      then form is evaluated
    finally the catch entry is removed.

  *catch is still an nlambda since its arguments should not be evaluated
   before this routine is called.

   (catch form [tag]) is translated to (*catch 'tag form) by a macro.
 */
lispval
Ncatch()
{
	register lispval tag;
	pbuf pb;
	Savestack(3);		/* save stack pointers */

	if((TYPE(lbot->val))!=DTPR) return(nil);
	protect(tag = eval(lbot->val->d.car));  /* protect tag from gc */

	errp = Pushframe(F_CATCH,tag,nil);

	switch(retval) {

	case C_THROW: 	/*
		       	 * value thrown is in lispretval
		       	 */
			break;

	case C_INITIAL: /*
			 * calculate value of expression
			 */
			 lispretval = eval(lbot->val->d.cdr->d.car);
	}
			
			
	errp = Popframe();
	Restorestack();
	return(lispretval);
}
/* (errset form [flag])  
   if present, flag determines if the error message will be printed
   if an error reaches the errset.
   if no error occurs, errset returns a list of one element, the 
    value returned from form.
   if an error occurs, nil is usually returned although it could
    be non nil if err threw a non nil value 
 */

lispval Nerrset()
{
	lispval temp,flag;
	pbuf pb;
	Savestack(0);

	if(TYPE(lbot->val) != DTPR) return(nil);	/* no form */

	/* evaluate and save flag first */
	flag = lbot->val->d.cdr;
	if(TYPE(flag) == DTPR) flag = eval(flag->d.car); 
	else flag = tatom; 	/* if not present , assume t */
	protect(flag);

	errp = Pushframe(F_CATCH,Verall,flag);

	switch(retval) {

	case C_THROW: 	/*
			 * error thrown to this routine, value thrown is
			 * in lispretval
			 */
			break;

	case C_INITIAL:	/*
			 * normally just evaluate expression and listify it.
			 */
			temp = eval(lbot->val->d.car);
			protect(temp);
			(lispretval = newdot())->d.car = temp;
			break;
	}

	errp = Popframe();
	Restorestack();
	return(lispretval);
}
	
/* this was changed from throw to *throw 21nov79
   it is now a lambda and really should be called Lthrow
*/
lispval
Nthrow()
{
	switch(np-lbot) {
	case 0:
		protect(nil);
	case 1:
		protect(nil);
	case 2: break;
	default:
		argerr("throw");
	}
	Inonlocalgo(C_THROW,lbot->val,(lbot+1)->val);
	/* NOT REACHED */
}



/* Ngo ******************************************************************/
/* First argument only is checked - and must be an atom or evaluate	*/
/* to one.								*/
lispval
Ngo() 
{
    register lispval temp;
    chkarg(1,"go");

    temp = (lbot->val)->d.car;
    if (TYPE(temp) != ATOM)
    {
	temp = eval(temp);
	while(TYPE(temp) != ATOM) 
	  temp = errorh1(Vermisc,"Illegal tag to go to",nil,TRUE, 0,lbot->val);
    }
    Inonlocalgo(C_GO,temp,nil);
    /* NOT REACHED */
}


/* Nreset ***************************************************************/
/* All arguments are ignored.  This just returns-from-break to depth 0.	*/
lispval
Nreset()
{
    Inonlocalgo(C_RESET,inewint(0),nil);
}



/* Nbreak ***************************************************************/
/* If first argument is not nil, this is evaluated and printed.  Then	*/
/* error is called with the "breaking" message.				*/

lispval
Nbreak()
{
	register lispval hold; register FILE *port;
	port = okport(Vpoport->a.clb,stdout);
	fprintf(port,"Breaking:");

	if ((hold = lbot->val) != nil && ((hold = hold->d.car) != nil))
	{
		printr(hold,port);
	}
	putc('\n',port);
	dmpport(port);
	return(errorh(Verbrk,"",nil,TRUE,0));
}


/* Nexit ****************************************************************/
/* Just calls lispend with no message.					*/
Nexit()
	{
	lispend("");
	}


/* Nsys *****************************************************************/
/* Just calls lispend with no message.					*/

lispval
Nsys()
	{
	lispend("");
	}




lispval
Ndef() {
	register lispval arglist, body, name, form;
	
	form = lbot->val;
	name = form->d.car;
	body = form->d.cdr->d.car;
	arglist = body->d.cdr->d.car;
	if((TYPE(arglist))!=DTPR && arglist != nil)
		error("Warning: defining function with nonlist of args",
			TRUE);
	name->a.fnbnd = body;
	return(name);
}


lispval
Nquote()
{
	return((lbot->val)->d.car);
}


lispval
Nsetq()
{	register lispval handy, where, value;
	register int lefttype;

	value = nil;
	
	for(where = lbot->val; where != nil; where = handy->d.cdr) {
		handy = where->d.cdr;
		if((TYPE(handy))!=DTPR)
			error("odd number of args to setq",FALSE);
		if((lefttype=TYPE(where->d.car))==ATOM) {
			if(where->d.car==nil)
				error("Attempt to set nil",FALSE);
			where->d.car->a.clb = value = eval(handy->d.car);
		 }else if(lefttype==VALUE)
			where->d.car->l = value = eval(handy->d.car);
		else errorh1(Vermisc,
			    "Can only setq atoms or values",nil,FALSE,0,
			    		where->d.car);
	}
	return(value);
}


lispval
Ncond()
{
	register lispval  where, last;

	where = lbot->val;
	last = nil;
	for(;;) {
		if ((TYPE(where))!=DTPR)
			break;
		if ((TYPE(where->d.car))!=DTPR)
			break;
		if ((last=eval((where->d.car)->d.car)) != nil)
			break;
		where = where->d.cdr;
	}

	if ((TYPE(where)) != DTPR)
			return(nil);
	where = (where->d.car)->d.cdr;
	while ((TYPE(where))==DTPR) {
			last = eval(where->d.car);
			where = where->d.cdr;
	}
	return(last);
}

lispval
Nand()
{
	register lispval current, temp;

	current = lbot->val;
	temp = tatom;
	while (current != nil)
		if ( (temp = current->d.car)!=nil && (temp = eval(temp))!=nil) 
			current = current->d.cdr;
		else {
			current = nil;
			temp = nil;
		}
	return(temp);
}


lispval
Nor()
{
	register lispval current, temp;

	current = lbot->val;
	temp = nil;
	while (current != nil)
		if ( (temp = eval(current->d.car)) == nil)
			current = current->d.cdr;
		else
			break;
	return(temp);
}
