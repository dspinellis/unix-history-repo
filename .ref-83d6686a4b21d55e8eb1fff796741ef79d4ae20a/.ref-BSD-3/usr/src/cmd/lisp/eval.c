#include "global.h"
/************************************************************************/
/*									*/
/*   file: eval.i							*/
/*   contents: evaluator and namestack maintenance routines		*/
/*									*/
/************************************************************************/


/* eval *****************************************************************/
/* returns the value of the pointer passed as the argument.		*/

lispval
eval(actarg)
lispval actarg;
{
#define argptr handy
	register lispval a = actarg;
	register lispval handy;
	register struct nament *namptr;
	register struct argent *workp;
	register struct argent *lbot;
	register struct argent *np;
	struct argent *poplbot;
	struct nament *oldbnp = bnp;
	lispval Ifcall(), Iarray();

	/*debugging 
	printf("Eval:");
	printr(a,stdout);
	fflush(stdout);  */
	switch (TYPE(a))
	{
	case ATOM:
		handy = a->clb;
		if(handy==CNIL) {
			handy = errorh(Vermisc,"Unbound Variable:",nil,TRUE,0,a);
		}
		return(handy);

	case VALUE:
		return(a->l);

	case DTPR:
		(np++)->val = a;	/* push form on namestack */
		lbot = np;		/* define beginning of argstack */
		oldbnp = bnp;		/* remember start of bind stack */
		a = a->car;		/* function name or lambda-expr */
		for(EVER)
			{
			switch(TYPE(a))
				{
			case ATOM:
					/*  get function binding  */
				if(a->fnbnd==nil && a->clb!=nil) {
					a=a->clb;
					if(TYPE(a)==ATOM)
						a=a->fnbnd;
				} else
					a = a->fnbnd;
				break;
			case VALUE:
				a = a->l;	/*  get value  */
				break;
				}

			vtemp = (CNIL-1);       /* sentinel value for error test */

		funcal:	switch (TYPE(a))
				{
			case BCD:	/* function */
				argptr = actarg->cdr;

				/* decide whether lambda, nlambda or
				   macro and push args onto argstack
				   accordingly.				*/

				if(a->discipline==nlambda) {
					(np++)->val = argptr;
					TNP;
				}else if(a->discipline==macro) {
					(np++)->val = actarg;
					TNP;
				} else for(;argptr!=nil; argptr = argptr->cdr) {
					(np++)->val = eval(argptr->car);
					TNP;
				}
				/* go for it */

				if(TYPE(a->discipline)==INT)
					vtemp = Ifcall(a);
				else
					vtemp = (*(lispval (*)())(a->entry))();
				break;

			case ARRAY:
				vtemp = Iarray(a,actarg->cdr);
				break;


			case DTPR:
					/* push args on argstack according to
					   type				*/

				argptr = a->car;
				if (argptr==lambda) {
					for(argptr = actarg->cdr;
					    argptr!=nil; argptr=argptr->cdr) {
						
						(np++)->val = eval(argptr->car);
						TNP;
					}
				} else if (argptr==nlambda) {
					(np++)->val = actarg->cdr;
					TNP;
				} else if(argptr==macro) {
					(np++)->val = actarg;
					TNP;
				} else if(argptr==lexpr) {
					for(argptr = actarg->cdr;
					    argptr!=nil; argptr=argptr->cdr) {
						
						(np++)->val = eval(argptr->car);
						TNP;
					}
					handy = newdot();
					handy->car = (lispval)lbot;
					handy->cdr = (lispval)np;
					PUSHDOWN(lexpr_atom,handy);
					lbot = np;
					(np++)->val = inewint(((lispval *)handy->cdr) - (lispval *)handy->car);

				} else break;	/* something is wrong - this isn't a proper function */

				argptr = (a->cdr)->car;
				namptr =  bnp;
				workp = lbot;
				if(bnp + (np - lbot)> bnplim)
					binderr();
				for(;argptr != (lispval)nil;
				     workp++,argptr = argptr->cdr)	/* rebind formal names (shallow) */
				{
					if(argptr->car==nil)
						continue;
					/*if(((namptr)->atm = argptr->car)==nil)
						error("Attempt to lambda bind nil",FALSE);*/
					namptr->atm = argptr->car;
					if (workp < np) {
						namptr->val = namptr->atm->clb;
						namptr->atm->clb = workp->val;
					} else
						bnp = namptr,
						error("Too few actual parameters",FALSE);
					namptr++;
				}
				bnp = namptr;
				if (workp < np)
					error("Too many actual parameters",FALSE);

				/* execute body, implied prog allowed */

				for (handy = a->cdr->cdr;
					handy != nil;
					handy = handy->cdr) {
						vtemp = eval(handy->car);
					}
				}
			if (vtemp != (CNIL-1))
				/* if we get here with a believable value, */
				/* we must have executed a function. */
				{
				popnames(oldbnp);

				/* in case some clown trashed t */

				tatom->clb = (lispval) tatom;
				if(a->car==macro) return(eval(vtemp));
					/* It is of the most wonderful 
					   coincidence that the offset
					   for car is the same as for
					   discipline so we get bcd macros
					   for free here ! */
				else return(vtemp);
				}
			popnames(oldbnp);
			a = (lispval) errorh(Vermisc,"BAD FUNCTION",nil,TRUE,0,actarg);
			}

		}
	return(a);	/* other data types are considered constants */
	}




/* popnames *************************************************************/
/* removes from the name stack all entries above the first argument.	*/
/* routine should usually be used to clean up the name stack as it	*/
/* knows about the special cases.  np is returned pointing to the	*/
/* same place as the argument passed.					*/
lispval
popnames(llimit)
register struct nament *llimit;
{
	register struct nament *rnp;

	for(rnp = bnp - 1; rnp >= llimit; rnp--)
		rnp->atm->clb = rnp->val;
	bnp = llimit;
}


/************************************************************************/
/*									*/
/*   file: apply.c							*/
/*	Caveat -- Work in Progress -- not guaranteed! not tested!
/*									*/
/* apply  ***************************************************************/
lispval
Lapply()
{
	register lispval a;
	register lispval handy;
	register struct argent *workp;
	register struct nament *namptr;
	register struct argent *lbot;
	register struct argent *np;
	lispval vtemp;
	struct nament *oldbnp = bnp;
	struct argent *oldlbot = lbot; /* Bottom of my frame! */

	a = lbot->val;
	argptr = lbot[1].val;
	if(np-lbot!=2)
		errorh(Vermisc,"Apply: Wrong number of args.",nil,FALSE,
		       999,a,argptr);
	if(TYPE(argptr)!=DTPR && argptr!=nil)
		argptr = errorh(Vermisc,"Apply: non-list of args",nil,TRUE,
				998,argptr);
	(np++)->val = a;	/* push form on namestack */
	TNP;
	lbot = np;		/* bottom of current frame */
	for(EVER)
		{
		if (TYPE(a) == ATOM) a = a->fnbnd;
			/* get function defn (unless calling form */
			/* is itself a lambda-expr) */
		vtemp = CNIL;			/* sentinel value for error test */
		switch (TYPE(a))
			{
		case BCD:	/* printf("BCD\n");*/
				/* push arguments - value of a */
			if(a->discipline==nlambda || a->discipline==macro) {
				(np++)->val=argptr;
				TNP;
			} else for (; argptr!=nil; argptr = argptr->cdr) {
				(np++)->val=argptr->car;
				TNP;
			}

			vtemp = (*(lispval (*)())(a->entry))(); /* go for it */
			break;

		case ARRAY:
			vtemp = Iarray(a,argptr);
			break;


		case DTPR:
			if (a->car==nlambda || a->car==macro) {
				(np++)->val = argptr;
				TNP;
			} else if (a->car==lambda)
				for (; argptr!=nil; argptr = argptr->cdr) {
					(np++)->val = argptr->car;
					TNP;
				}
			else if(a->car==lexpr) {
				for (; argptr!=nil; argptr = argptr->cdr) {
					
					(np++)->val = argptr->car;
					TNP;
				}
				handy = newdot();
				handy->car = (lispval)lbot;
				handy->cdr = (lispval)np;
				PUSHDOWN(lexpr_atom,handy);
				lbot = np;
				(np++)->val = inewint(((lispval *)handy->cdr) - (lispval *)handy->car);

			} else break;	/* something is wrong - this isn't a proper function */
			rebind(a->cdr->car,lbot);
			np = lbot;
			for (handy = a->cdr->cdr;
				handy != nil;
				handy = handy->cdr) {
					vtemp = eval(handy->car);	/* go for it */
				}
			}
		if (vtemp != CNIL)
			/* if we get here with a believable value, */
			/* we must have executed a function. */
			{
			popnames(oldbnp);

			/* in case some clown trashed t */

			tatom->clb = (lispval) tatom;
			return(vtemp);
			}
		popnames(oldbnp);
		printr(oldlbot->val,stdout);
		a = (lispval) error("BAD FUNCTION",TRUE);
	}
	/*NOT REACHED*/
}


/*
 * Rebind -- rebind formal names
 */
rebind(argptr,workp)
register lispval argptr;		/* argptr points to list of atoms */
register struct argent * workp;		/* workp points to position on stack
					   where evaluated args begin */
{
	register lispval vtemp;
	register struct nament *namptr = bnp;
	register struct argent *lbot;
	register struct argent *np;

	for(;argptr != (lispval)nil;
	     workp++,argptr = argptr->cdr)  /* rebind formal names (shallow) */
	{
		if(argptr->car==nil)
			continue;
		namptr->atm = argptr->car;
		if (workp < np) {
			namptr->val = namptr->atm->clb;
			namptr->atm->clb = workp->val;
		} else
			bnp = namptr,
			error("Too few actual parameters",FALSE);
		namptr++;
		if(namptr > bnplim)
			binderr();
	}
	bnp = namptr;
	if (workp < np)
		error("Too many actual parameters",FALSE);
}

lispval
Lfuncal()
{
	register lispval a;
	register lispval handy; 
	register struct argent *oldlbot;
	register struct nament **namptr;
	register struct argent *lbot;
	register struct argent *np;

	lispval Ifcall(),Llist(),Iarray();
	lispval vtemp;
	struct nament *oldbnp = bnp;
	int typ;
	extern lispval end[];

	/*debugging stufff 
	printf("In funcal: ");
	printr(lbot->val,stdout);
	fflush(stdout); 
	printf("\n"); */

	oldlbot = lbot;		/* bottom of my namestack frame */
	a = lbot->val;		/* function I am evaling.	*/
	lbot++;

	for(EVER)
	{
		typ = TYPE(a);
		if (typ == ATOM) a = a->fnbnd, typ = TYPE(a);

			/* get function defn (unless calling form */
			/* is itself a lambda-expr) */
		vtemp = CNIL;			/* sentinel value for error test */
		switch (typ) {
		case ARRAY:
			vtemp = Iarray(a,Llist());
			break;
		case BCD:
			if(a->discipline==nlambda)
			    {   if(np==lbot) protect(nil);  /* default is nil */
				while(np-lbot!=1 || (lbot->val != nil &&
						  TYPE(lbot->val)!=DTPR)) {
					lbot->val = error("Bad funcall arg(s) to fexpr.",TRUE);
					np = lbot+1;
					}
			    }
			/* go for it */

			if(TYPE(a->discipline)==INT)
				vtemp = Ifcall(a);
			else
				vtemp = (*(lispval (*)())(a->entry))();
			if(a->discipline==macro)
				vtemp = eval(vtemp);
			break;


		case DTPR:
			if (a->car == lambda) {
				;/* VOID */
			} else if (a->car == nlambda || a->car==macro) {
				if( np==lbot ) protect(nil);	/* default */
				while(np-lbot!=1 || (lbot->val != nil &&
						  TYPE(lbot->val)!=DTPR)) {
					lbot->val = error("Bad funcall arg(s) to fexpr.",TRUE);
					np = lbot+1;
					}
			} else if (a->car == lexpr) {
				handy = newdot();
				handy->car = (lispval) lbot;
				handy->cdr = (lispval) np;
				PUSHDOWN(lexpr_atom,handy);
				lbot = np;
				(np++)->val = inewint(((lispval *)handy->cdr) - (lispval *)handy->car);
			} else break;		/* something is wrong - this isn't a proper function */
			rebind(a->cdr->car,lbot);
			np = lbot;
			for (handy = a->cdr->cdr;
				handy != nil;
				handy = handy->cdr) {
					vtemp = eval(handy->car);	/* go for it */
				}
			if(a->car==macro)
				vtemp = eval(vtemp);
		}
		if (vtemp != CNIL)
			/* if we get here with a believable value, */
			/* we must have executed a function. */
			{
			popnames(oldbnp);

			/* in case some clown trashed t */

			tatom->clb = (lispval) tatom;
			/*debugging
			if(a>(lispval) end){printf(" leaving:");
			printr(a,stdout);
			fflush(stdout);} */
			return(vtemp);
			}
		popnames(oldbnp);
		printr(oldlbot->val,stdout);
		a = (lispval) error("BAD FUNCTION",TRUE);

	}
	/*NOT REACHED*/
}

/* protect **************************************************************/
/* pushes the first argument onto namestack, thereby protecting from gc */
lispval
protect(a)
lispval a;
{
	/* (np++)->val = a;
	   if (np >=  nplim)
		namerr();
	 */
	asm("	movl	4(ap),(r6)+");
	asm("	cmpl	r6,_nplim");
	asm("	jlss	out1");
	asm("	calls	$0,_namerr");
	asm("out1:	ret");
	}


/* unprot ****************************************************************/
/* returns the top thing on the name stack.  Underflow had better not	*/
/* occur.								*/
lispval
unprot()
	{
	asm("	movl	-(r6),r0");
	}

lispval
linterp()
	{
	error("BYTE INTERPRETER CALLED ERRONEOUSLY",FALSE);
	}

/* Undeff - called from qfuncl when it detects a call to a undefined
	function from compiled code, we print out a message and
	dont allow continuation
*/
lispval
Undeff(atmn)
lispval atmn;
{
	printf("\n%s - ",atmn->pname);
	error("Undefined function called from compiled code",FALSE);
}
bindfix(firstarg)
lispval firstarg;
{
	register lispval *argp = &firstarg;
	register struct nament *mybnp = bnp;
	while(*argp != nil) {
		mybnp->atm = *argp++;
		mybnp->val = mybnp->atm->clb;
		mybnp->atm->clb = *argp++;
		bnp = mybnp++;
	}
}
