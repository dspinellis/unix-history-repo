static char *sccsid = "@(#)fex2.c	34.1 10/3/80";

#include "global.h"
#define NDOVARS 30
#include <assert.h>
/*
 * Ndo  maclisp do function.
 */
lispval
Ndo()
{
	register lispval current, where, handy;
	register struct nament *mybnp;
	register struct argent *lbot, *np;
	lispval atom, temp;
	lispval body, endtest, endform, varstuf, renewals[NDOVARS] ;
	struct argent *start, *last, *getem,  *savedlbot; 
	struct nament *savedbnp, *lastbnd;
	int count, index, saveme[SAVSIZE], virgin = 1;
	long myerrp; extern long errp;

	savedlbot = lbot;
	myerrp = errp;
	savedbnp = bnp;
	getexit(saveme);		/* common nonlocal return */
	if(retval = setexit()) {
		errp = myerrp;
		if(retval == BRRETN) {
			resexit(saveme);
			lbot = savedlbot;
			popnames(savedbnp);
			return((lispval) contval);
		} else {
			resexit(saveme);
			lbot = savedlbot;
			reset(retval);
		}
	}
	current = lbot->val;
	varstuf = current->d.car;
	switch( TYPE(varstuf) ) {

	case ATOM:			/* This is old style maclisp do;
					   atom is var, cadr(current) = init;
					   caddr(current) = repeat etc. */
		atom = varstuf;
		if(varstuf==nil) goto newstyle;
		bnp->atm = atom;	/* save current binding of atom */
		bnp++->val = atom->a.clb;
		if(bnp > bnplim)
			binderr();
		current = current->d.cdr;
		atom->a.clb = eval(current->d.car);
					/* Init var.	    */
		*renewals = (current = current->d.cdr)->d.car;
					/* get repeat form  */
		endtest	= (current = current->d.cdr)->d.car;
		body = current->d.cdr;

		while(TRUE) {
			if(eval(endtest)!=nil) {
				resexit(saveme);
				popnames(savedbnp);
				return(nil);
			}
			doprog(body);
			atom->a.clb = eval(*renewals);
		}
	

	newstyle:
	case DTPR:			/* New style maclisp do; atom is
					   list of things of the form
					   (var init repeat)		*/
		count = 0;
		start = np;
		for(where = varstuf; where != nil; where = where->d.cdr) {
					/* do inits and count do vars. */
					/* requires "simultaneous" eval
					   of all inits			*/
			handy = where->d.car->d.cdr;
			temp = nil;
			if(handy !=nil)
				temp = eval(handy->d.car);
			protect(temp);
			count++;
		}
		if(count > NDOVARS)
			error("More than 15 do vars",FALSE);
		bnp += count;
		if(bnp >= bnplim) {
			bnp = savedbnp;
			namerr();
		}
		last = np;
		where = varstuf;
		mybnp = savedbnp;
		getem = start;
		for(index = 0; index < count; index++) {

			handy = where->d.car;
					/* get var name from group	*/
			atom = handy->d.car;
			mybnp->atm = atom;
			mybnp->val = atom->a.clb;
					/* Swap current binding of atom
					   for init val pushed on stack */

			atom->a.clb = getem++->val;
					/* As long as we are down here in the
					   list, save repeat form	*/
			handy = handy->d.cdr->d.cdr;
			if(handy==nil)
				handy = CNIL;  /* be sure not to rebind later */
			else
				handy = handy->d.car;
			renewals[index] = handy;

					/* more loop "increments" */
			where = where->d.cdr;
			mybnp++;
		}
					/* Examine End test and End form */
		current = current->d.cdr;
		handy = current->d.car;
		body = current->d.cdr;
		if (handy == nil) {
			doprog(body);
			popnames(savedbnp);
			resexit(saveme);
			return(nil);
		}
		endtest = handy->d.car;
		endform = handy->d.cdr;
					/* The following is the loop: */
	loop:
		if(eval(endtest)!=nil) {
			for(handy = nil; endform!=nil; endform = endform->d.cdr){
				handy = eval(endform->d.car);
			}
			resexit(saveme);
			popnames(savedbnp);
			return(handy);
		}
		doprog(body);
					/* Simultaneously eval repeat forms */
		for(index = 0; index < count; index++) {

			temp = renewals[index];
			if (temp == nil || temp == CNIL)
				protect(temp);
			else
				protect(eval(temp));
		}
		getem = (np = last);
					/* now simult. rebind all the atoms */
		mybnp = savedbnp;
		for(index = 0; index < count; index++, getem++) {
		   if( (getem)->val != CNIL )  /* if this atom has a repeat form */
			mybnp->atm->a.clb = (getem)->val;  /* rebind */
			mybnp++;
		}
		goto loop;
	}
}
doprog(body)
register lispval body;
	{
	int	saveme[SAVSIZE];
	register lispval where, temp;
	/*register struct nament *savednp = np, *savedlbot = lbot;*/
	extern long errp; long myerrp = errp;
	struct nament *savedbnp = bnp;
	snpand(3);

	where = body;
	getexit(saveme);
	if(retval = setexit()) {
		errp = myerrp;
		switch (retval)	{

		default:	resexit(saveme);
				reset(retval);

		case BRGOTO:
			for(where = body;
				where->d.car != (lispval) contval; where = where->d.cdr) {

				if(where==nil) {
					resexit(saveme);
					reset(retval);
				}
				/* np is automatically restored here by
				   virtue of being a register */
			}
			popnames(savedbnp);
		}
	}
	while (TYPE(where) == DTPR) {
		temp = where->d.car;
		if((TYPE(temp))!=ATOM) eval(temp);
		where = where->d.cdr;
	}
	resexit(saveme);
}
lispval
Nprogv()
{
	register lispval where, handy;
	register struct nament *namptr;
	register struct argent *vars, *lbot, *np;
	struct argent *start;
	struct nament *oldbnp = bnp;

	where = lbot->val;
	protect(eval(where->d.car));		/* list of vars = lbot[1].val */
	protect(eval((where = where->d.cdr)->d.car));
						/* list of vals */
	handy = lbot[2].val;
	start = np; namptr = oldbnp;
						/* simultaneous eval of all
						   args */
	for(;handy!=nil; handy = handy->d.cdr) {
		(np++)->val = eval(handy->d.car);
		TNP;
	}
	asm("# Here is where rebinding is done");
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
	return(handy);
}

lispval
Nprogn()
{
	register lispval result, where;
	snpand(2);

	result = nil;
	for(where = lbot->val; where != nil; where = where->d.cdr)
		result = eval(where->d.car);
	return(result);


}
lispval
Nprog2()
{
	register lispval result, where;
	snpand(2);

	where = lbot->val; 
	eval(where->d.car);
	result = eval((where = where->d.cdr)->d.car);
	protect(result);
	for(where = where->d.cdr; where != nil; where = where->d.cdr)
		eval(where->d.car);
	return(result);
}
