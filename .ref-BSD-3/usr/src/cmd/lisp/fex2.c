#include "global.h"
#define NDOVARS 15
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
	int myerrp; extern int errp;

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
	varstuf = current->car;
	switch( TYPE(varstuf) ) {

	case ATOM:			/* This is old style maclisp do;
					   atom is var, cadr(current) = init;
					   caddr(current) = repeat etc. */
		atom = varstuf;
		if(varstuf==nil) goto newstyle;
		bnp->atm = atom;	/* save current binding of atom */
		bnp++->val = atom->clb;
		if(bnp > bnplim)
			binderr();
		current = current->cdr;
		atom->clb = eval(current->car);
					/* Init var.	    */
		*renewals = (current = current->cdr)->car;
					/* get repeat form  */
		endtest	= (current = current->cdr)->car;
		body = current->cdr;

		while(TRUE) {
			if(eval(endtest)!=nil) {
				resexit(saveme);
				popnames(savedbnp);
				return(nil);
			}
			doprog(body);
			atom->clb = eval(*renewals);
		}
	

	newstyle:
	case DTPR:			/* New style maclisp do; atom is
					   list of things of the form
					   (var init repeat)		*/
		count = 0;
		start = np;
		for(where = varstuf; where != nil; where = where->cdr) {
					/* do inits and count do vars. */
					/* requires "simultaneous" eval
					   of all inits			*/
			handy = where->car->cdr;
			temp = nil;
			if(handy !=nil)
				temp = eval(handy->car);
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

			handy = where->car;
					/* get var name from group	*/
			atom = handy->car;
			mybnp->atm = atom;
			mybnp->val = atom->clb;
					/* Swap current binding of atom
					   for init val pushed on stack */

			atom->clb = getem++->val;
					/* As long as we are down here in the
					   list, save repeat form	*/
			handy = handy->cdr->cdr;
			if(handy==nil)
				handy = CNIL;  /* be sure not to rebind later */
			else
				handy = handy->car;
			renewals[index] = handy;

					/* more loop "increments" */
			where = where->cdr;
			mybnp++;
		}
					/* Examine End test and End form */
		current = current->cdr;
		handy = current->car;
		body = current->cdr;
		if (handy == nil) {
			doprog(body);
			popnames(savedbnp);
			resexit(saveme);
			return(nil);
		}
		endtest = handy->car;
		endform = handy->cdr;
					/* The following is the loop: */
	loop:
		if(eval(endtest)!=nil) {
			for(handy = nil; endform!=nil; endform = endform->cdr){
				handy = eval(endform->car);
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
			mybnp->atm->clb = (getem)->val;  /* rebind */
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
	extern int errp; int myerrp = errp;
	struct nament *savedbnp = bnp;
	snpand(2);

	where = body;
	getexit(saveme);
	if(retval = setexit()) {
		errp = myerrp;
		switch (retval)	{

		default:	resexit(saveme);
				reset(retval);

		case BRGOTO:
			for(where = body;
				where->car != (lispval) contval; where = where->cdr) {

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
		temp = where->car;
		if((TYPE(temp))!=ATOM) eval(temp);
		where = where->cdr;
	}
	resexit(saveme);
}
lispval
Nprogv()
{
	register lispval argptr, where, handy, atoms;
	register struct argent *lbot, *np;
	struct argent *namptr, *start;
	struct nament *oldbnp = bnp;

	where = lbot->val;
	protect(eval(where->car));		/* list of vars */
	atoms = lbot[1].val;
	protect(eval((where = where->cdr)->car));
						/* list of vals */
	handy = lbot[2].val;
	start = np;
	for(;handy!=nil; handy = handy->cdr) {
		(np++)->val = eval(handy->car);
		TNP;
	}
	rebind(atoms,start);
	handy = nil;
	for(where = where->cdr; where != nil; where = where->cdr)
		handy = eval(where->car);
	popnames(oldbnp);
	return(handy);
}

lispval
Nprogn()
{
	register lispval result, where;
	snpand(2);

	result = nil;
	for(where = lbot->val; where != nil; where = where->cdr)
		result = eval(where->car);
	return(result);


}
lispval
Nprog2()
{
	register lispval result, where;
	snpand(2);

	where = lbot->val; 
	eval(where->car);
	result = eval((where = where->cdr)->car);
	protect(result);
	for(where = where->cdr; where != nil; where = where->cdr)
		eval(where->car);
	return(result);
}
