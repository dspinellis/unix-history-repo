
#ifndef lint
static char *rcsid =
   "$Header: fex2.c,v 1.3 83/09/07 17:55:38 sklower Exp $";
#endif

/*					-[Mon Jan 31 21:54:52 1983 by layer]-
 * 	fex2.c				$Locker:  $
 * nlambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"
#define NDOVARS 30
#include "frame.h"

/*
 * Ndo  maclisp do function.
 */
lispval
Ndo()
{
	register lispval current, where, handy;
	register struct nament *mybnp;
	lispval temp, atom;
	lispval body, endtest, endform, varstuff, renewals[NDOVARS] ;
	struct argent *getem, *startnp;  
	struct nament *savedbnp = bnp;
	int count, repeatdo, index;
	extern struct frame *errp;
	pbuf pb;
	Savestack(3);

	current = lbot->val;
	varstuff = current->d.car;

	switch( TYPE(varstuff) ) {

	case ATOM:			/* This is old style maclisp do;
					   atom is var, cadr(current) = init;
					   caddr(current) = repeat etc. */
		if(varstuff==nil) goto newstyle;
		current = current->d.cdr;	/* car(current) is now init */
		PUSHDOWN(varstuff,eval(current->d.car));
					/* Init var.	    */
		*renewals = (current = current->d.cdr)->d.car;
					/* get repeat form  */
		endtest	= (current = current->d.cdr)->d.car;
		body = current->d.cdr;

		errp = Pushframe(F_PROG,nil,nil);

		switch (retval) {
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
				where = body;
				while ((TYPE(where) == DTPR) 
					& (where->d.car != lispretval))
				where = where->d.cdr;
				if (where->d.car == lispretval) {
					popnames(errp->svbnp);
					where = where->d.cdr;
					goto singbody;
				}
				/* label not found in this prog, must 
				 * go up to higher prog
				 */
				Inonlocalgo(C_GO,lispretval,nil);

				/* NOT REACHED */

		    case C_INITIAL: break;  	/* fall through */

		}

	    singtop:
		    if(eval(endtest)!=nil) {
			errp = Popframe();
			popnames(savedbnp);
			return(nil);
		    }
		    where = body;
		    
	    singbody:
		    while (TYPE(where) == DTPR)
		    {
			temp = where->d.car;
			if((TYPE(temp))!=ATOM) eval(temp);
			where = where->d.cdr;
		    }
		    varstuff->a.clb = eval(*renewals);
		    goto singtop;
	

	newstyle:
	case DTPR:			/* New style maclisp do; atom is
					   list of things of the form
					   (var init repeat)		*/
		count = 0;
		startnp = np;
		for(where = varstuff; where != nil; where = where->d.cdr) {
					/* do inits and count do vars. */
					/* requires "simultaneous" eval
					   of all inits			*/
		        while (TYPE(where->d.car) != DTPR)
			  where->d.car =
			     errorh1(Vermisc,"do: variable forms must be lists ",
			     nil,TRUE,0,where->d.car);
			handy = where->d.car->d.cdr;
			temp = nil;
			if(handy !=nil)
				temp = eval(handy->d.car);
			protect(temp);
			count++;
		}
		if(count > NDOVARS)
			error("More than 15 do vars",FALSE);
		where = varstuff;
		getem = startnp;	/* base of stack of init forms */
		for(index = 0; index < count; index++) {

			handy = where->d.car;
					/* get var name from group	*/

			atom = handy->d.car;
			while((TYPE(atom) != ATOM) || (atom == nil))
			  atom = errorh1(Vermisc,"do variable must be a non nil symbol ",
						    nil,TRUE,0,atom);
			PUSHDOWN(atom,getem->val);
			getem++;
			handy = handy->d.cdr->d.cdr;
			if(handy==nil)
				handy = CNIL;  /* be sure not to rebind later */
			else
				handy = handy->d.car;
			renewals[index] = handy;

					/* more loop "increments" */
			where = where->d.cdr;
		}
		np = startnp;		/* pop off all init forms */
					/* Examine End test and End form */
		current = current->d.cdr;
		handy = current->d.car;
		body = current->d.cdr;

		/* 
		 * a do form with a test of nil just does the body once
		 * and returns nil
		 */
		if (handy == nil) repeatdo = 1; /* just do it once */
		else repeatdo = -1;		/* do it forever   */

		endtest = handy->d.car;
		endform = handy->d.cdr;

		where = body;

		errp = Pushframe(F_PROG,nil,nil);
		while(TRUE) {

		    switch (retval) {
		    case C_RET:	/*
				 * returning from this prog, value to return
				 * is in lispretval
				 */
				errp = Popframe();
				popnames(savedbnp);
				Restorestack();
				return(lispretval);

		    case C_GO:	/*
				 * going to a certain label, label to go to in
				 * in lispretval
				 */
				where = body;
				while ((TYPE(where) == DTPR) 
					& (where->d.car != lispretval))
				where = where->d.cdr;
				if (where->d.car == lispretval) {
					popnames(errp->svbnp);
					where = where->d.cdr;
					goto bodystart;
				}
				/* label not found in this prog, must 
				 * go up to higher prog
				 */
				Inonlocalgo(C_GO,lispretval,nil);

				/* NOT REACHED */

		    case C_INITIAL: break;  	/* fall through */

		    }

	    loop:
	    	    np = startnp;	/* is bumped when doing repeat forms */

		    if((repeatdo-- == 0) || (eval(endtest) !=nil)) {
			for(handy = nil; endform!=nil; endform = endform->d.cdr)
			{
				handy = eval(endform->d.car);
			}
			errp = Popframe();
			popnames(savedbnp);
			Restorestack();
			return(handy);
		    }
		    
	    bodystart:
		    while (TYPE(where) == DTPR)
		    {
			temp = where->d.car;
			if((TYPE(temp))!=ATOM) eval(temp);
			where = where->d.cdr;
		    }
		    where = body;
		    getem = np = startnp;
					/* Simultaneously eval repeat forms */
		    for(index = 0; index < count; index++) {
			temp = renewals[index];
			if (temp == nil || temp == CNIL)
				protect(temp);
			else
				protect(eval(temp));
		    }
					/* now simult. rebind all the atoms */
		    mybnp = savedbnp;
		    for(index = 0; index < count; index++) 
		    {
		       if( getem->val != CNIL )  /* if this atom has a repeat */
			mybnp->atm->a.clb = (getem)->val;  /* rebind */
			mybnp++;
			getem++;
		    }
		    goto loop;
	   	}
	    default:
		error("do: neither list nor atom follows do", FALSE);
	    }
		/* NOTREACHED */
}

lispval
Nprogv()
{
	register lispval where, handy;
	register struct nament *namptr;
	register struct argent *vars;
	struct nament *oldbnp = bnp;
	Savestack(4);

	where = lbot->val;
	protect(eval(where->d.car));		/* list of vars = lbot[1].val */
	protect(eval((where = where->d.cdr)->d.car));
						/* list of vals */
	handy = lbot[2].val;
	namptr = oldbnp;
						/* simultaneous eval of all
						   args */
	for(;handy!=nil; handy = handy->d.cdr) {
		(np++)->val = (handy->d.car);
		/*  Note, each element should not be reevaluated like it 
		 *  was  before.  - dhl */
		/* Before: (np++)->val = eval(handy->d.car);*/
		TNP;
	}
	/*asm("# Here is where rebinding is done");	 /* very cute */
	for(handy=lbot[1].val,vars=lbot+3; handy!=nil; handy=handy->d.cdr) {
	    namptr->atm = handy->d.car;
	    ++namptr;				/* protect against interrupts
						   while re-lambda binding */
	    bnp = namptr;
	    namptr[-1].atm = handy->d.car;
	    namptr[-1].val = handy->d.car->a.clb;
	    if(vars < np)
		handy->d.car->a.clb = vars++->val;
	    else
		handy->d.car->a.clb = nil;
	}
		
	handy = nil;
	for(where = where->d.cdr; where != nil; where = where->d.cdr)
		handy = eval(where->d.car);
	popnames(oldbnp);
	Restorestack();
	return(handy);
}

lispval
Nprogn()
{
	register lispval result, where;

	result = nil;
	for(where = lbot->val; where != nil; where = where->d.cdr)
		result = eval(where->d.car);
	return(result);


}
lispval
Nprog2()
{
	register lispval result, where;

	where = lbot->val; 
	eval(where->d.car);
	result = eval((where = where->d.cdr)->d.car);
	protect(result);
	for(where = where->d.cdr; where != nil; where = where->d.cdr)
		eval(where->d.car);
	np--;
	return(result);
}
lispval
typred(typ,ptr)
int 	typ;
lispval	ptr;

{   int tx;
	if ((tx = TYPE(ptr)) == typ) return(tatom);
	if ((tx == INT) && (typ == ATOM)) return(tatom);
	return(nil);
}

/*
 * function
 * In the interpreter, function is the same as quote
 */
lispval
Nfunction()
{
	if((lbot->val == nil) || (lbot->val->d.cdr != nil))
		argerr("function");
	return(lbot->val->d.car);
}
