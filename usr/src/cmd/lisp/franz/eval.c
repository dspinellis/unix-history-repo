static char *sccsid = "@(#)eval.c	35.3 7/9/81";

#include "global.h"
#include <signal.h>
#include "frame.h"

/************************************************************************/
/*									*/
/*   file: eval.c							*/
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
    struct nament *oldbnp = bnp;
    int dopopframe = FALSE;
    lispval Ifcall(), Iarray();
    Savestack(4);

    /*debugging 
    if (rsetsw && rsetatom->a.clb != nil) {
	printf("Eval:");
	printr(a,stdout);
	printf("\nrsetsw: %d evalhsw: %d\n", rsetsw, evalhsw);
	printf("*rset: ");
	printr(rsetatom->a.clb,stdout);
	printf(" evalhook: ");
	printr(evalhatom->a.clb,stdout);
	printf(" evalhook call flag^G: %d\n", evalhcallsw);
	fflush(stdout); 
    };  
    */

    /* check if an interrupt is pending	 and handle if so */
    if(sigintcnt > 0) sigcall(SIGINT);

    if (rsetsw && rsetatom->a.clb != nil)  /* if (*rset t) has been done */
    {    	
	if (evalhsw != nil && evalhatom->a.clb != nil)
	{
						/*if (sstatus evalhook t)
						    and evalhook non-nil */
	    if (evalhcallsw)
			/*if this is a call to evalhook, don't call evalhook
			  function, but clear evalhcallsw , so recursive 
			  calls to eval cause evalhook function to fire. */
		evalhcallsw = FALSE;
	    else {
		/* setup equivalent of (funcall evalhook <arg to eval>) */
		(np++)->val = a;		/* push form on namestack */
		lbot=np;			/* set up args to funcall */
		(np++)->val = evalhatom->a.clb; /* push evalhook's clb */
		(np++)->val = a;		/* eval's arg becomes
						           2nd arg to funcall */
		PUSHDOWN(evalhatom, nil);	/* lambda-bind evalhook to nil*/
		PUSHDOWN(funhatom, nil);	/* lambda-bind evalhook to nil*/
		funhcallsw = TRUE;		/* skip any funcall hook */
		handy = Lfuncal();		/* now call funcall */
		funhcallsw = FALSE;
		POP;
		POP;
		Restorestack();
		return(handy);
	    };
	}
	errp = Pushframe(F_EVAL,a);
	dopopframe = TRUE;	/* remember to pop later */
	if(retval == C_FRETURN)
	{
	    Restorestack();
	    errp = Popframe();
	    return(lispretval);
	}
    };
        
    switch (TYPE(a))
    {
    case ATOM:
	if (rsetsw && rsetatom->a.clb != nil && bptr_atom->a.clb != nil) {

	    struct nament *bpntr, *eval1bptr;
				    /* Both rsetsw and rsetatom for efficiency*/
				    /* bptr_atom set by second arg to eval1 */
	    eval1bptr = (struct nament *) bptr_atom->a.clb->d.cdr;
				    /* eval1bptr is bnp when eval1 was called;
				       if an atom was bound after this,
				       then its clb is valid */
	    for (bpntr = eval1bptr; bpntr < bnp; bpntr++)
		if (bpntr->atm==a) {
		    handy = a->a.clb;
		    goto gotatom;
		};		    /* Value saved in first binding of a,
				       if any, after pointer to eval1,
				       is the valid value, else use its clb */
	    for (bpntr = (struct nament *)bptr_atom->a.clb->d.car;
	      bpntr < eval1bptr; bpntr++)
		if (bpntr->atm==a) {
		    handy=bpntr->val;
		    goto gotatom;   /* Simply no way around goto here */
		};
	};
        handy = a->a.clb;
    gotatom:
        if(handy==CNIL) {
            handy = errorh(Vermisc,"Unbound Variable:",nil,TRUE,0,a);
        }
	if(dopopframe) errp = Popframe();
	Restorestack();
        return(handy);

    case VALUE:
	if(dopopframe) errp = Popframe();
	Restorestack();
        return(a->l);

    case DTPR:
        (np++)->val = a;		/* push form on namestack */
        lbot = np;			/* define beginning of argstack */
        /* oldbnp = bnp;		   redundant - Mitch Marcus */
        a = a->d.car;			/* function name or lambda-expr */
        for(EVER)
            {
            switch(TYPE(a))
                {
            case ATOM:
					/*  get function binding  */
                if(a->a.fnbnd==nil && a->a.clb!=nil) {
                    a=a->a.clb;
                    if(TYPE(a)==ATOM)
                        a=a->a.fnbnd;
                } else
                    a = a->a.fnbnd;
                break;
            case VALUE:
                a = a->l;		/*  get value  */
                break;
                }

            vtemp = (CNIL-1);       /* sentinel value for error test */

        funcal:    switch (TYPE(a))
                {
            case BCD:    /* function */
                argptr = actarg->d.cdr;

				    /* decide whether lambda, nlambda or
				       macro and push args onto argstack
				       accordingly.                */

                if(a->bcd.discipline==nlambda) {
                    (np++)->val = argptr;
                    TNP;
                } else if(a->bcd.discipline==macro) {
                    (np++)->val = actarg;
                    TNP;
                } else for(;argptr!=nil; argptr = argptr->d.cdr) {
                    (np++)->val = eval(argptr->d.car);
                    TNP;
                }
                /* go for it */

                if(TYPE(a->bcd.discipline)==STRNG)
                    vtemp = Ifcall(a);
                else
                    vtemp = (*(lispval (*)())(a->bcd.entry))();
                break;

            case ARRAY:
                vtemp = Iarray(a,actarg->d.cdr,TRUE);
                break;

            case DTPR:		    /* push args on argstack according to
				       type                */
                argptr = a->d.car;
                if (argptr==lambda) {
                    for(argptr = actarg->d.cdr;
                        argptr!=nil; argptr=argptr->d.cdr) {
                        
                        (np++)->val = eval(argptr->d.car);
                        TNP;
                    }
                } else if (argptr==nlambda) {
                    (np++)->val = actarg->d.cdr;
                    TNP;
                } else if (argptr==macro) {
                    (np++)->val = actarg;
                    TNP;
                } else if (argptr==lexpr) {
                    for(argptr = actarg->d.cdr;
                      argptr!=nil; argptr=argptr->d.cdr) {
                        
                        (np++)->val = eval(argptr->d.car);
                        TNP;
                    }
                    handy = newdot();
                    handy->d.car = (lispval)lbot;
                    handy->d.cdr = (lispval)np;
                    PUSHDOWN(lexpr_atom,handy);
                    lbot = np;
                    (np++)->val = inewint(((lispval *)handy->d.cdr) - (lispval *)handy->d.car);

                } else break;    /* something is wrong - this isn't a proper function */

                argptr = (a->d.cdr)->d.car;
                namptr =  bnp;
                workp = lbot;
                if(bnp + (np - lbot)> bnplim)
                    binderr();
                for(;argptr != (lispval)nil;
                     workp++,argptr = argptr->d.cdr)    /* rebind formal names (shallow) */
                {
                    if(argptr->d.car==nil)
                        continue;
                    /*if(((namptr)->atm = argptr->d.car)==nil)
                        error("Attempt to lambda bind nil",FALSE);*/
                    namptr->atm = argptr->d.car;
                    if (workp < np) {
                        namptr->val = namptr->atm->a.clb;
                        namptr->atm->a.clb = workp->val;
                    } else
                        bnp = namptr,
                        error("Too few actual parameters",FALSE);
                    namptr++;
                }
                bnp = namptr;
                if (workp < np)
                    error("Too many actual parameters",FALSE);

				    /* execute body, implied prog allowed */

                for (handy = a->d.cdr->d.cdr;
                    handy != nil;
                    handy = handy->d.cdr) {
                        vtemp = eval(handy->d.car);
                    }
                }
            if (vtemp != (CNIL-1)) {
				/* if we get here with a believable value, */
				/* we must have executed a function. */
                popnames(oldbnp);

                /* in case some clown trashed t */

                tatom->a.clb = (lispval) tatom;
                if(a->d.car==macro) 
		    vtemp = eval(vtemp);
                    /* It is of the most wonderful 
                       coincidence that the offset
                       for car is the same as for
                       discipline so we get bcd macros
                       for free here ! */
		if(dopopframe) errp = Popframe();
		Restorestack();
		return(vtemp);
	    }
            popnames(oldbnp);
            a = (lispval) errorh(Verundef,"eval: Undefined function ",nil,TRUE,0,actarg->d.car);
            }

        }
    if(dopopframe) errp = Popframe();
    Restorestack();
    return(a);    /* other data types are considered constants */
}




/* popnames *************************************************************/
/* removes from the name stack all entries above the first argument.    */
/* routine should usually be used to clean up the name stack as it    */
/* knows about the special cases.  np is returned pointing to the    */
/* same place as the argument passed.                    */
lispval
popnames(llimit)
register struct nament *llimit;
{
    register struct nament *rnp;

    for(rnp = bnp; --rnp >= llimit;)
        rnp->atm->a.clb = rnp->val;
    bnp = llimit;
}


/************************************************************************/
/*									*/
/*   file: apply.c							*/
/*    Caveat -- Work in Progress -- not guaranteed! not tested!		*/
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
    (np++)->val = a;    /* push form on namestack */
    TNP;
    lbot = np;        /* bottom of current frame */
    for(EVER)
        {
        if (TYPE(a) == ATOM) a = a->a.fnbnd;
					/* get function definition (unless
					   calling form is itself a lambda-
					   expression) */
        vtemp = CNIL;			/* sentinel value for error test */
        switch (TYPE(a)) {

        case BCD: 
					/* push arguments - value of a */
            if(a->bcd.discipline==nlambda || a->bcd.discipline==macro) {
                (np++)->val=argptr;
                TNP;
            } else for (; argptr!=nil; argptr = argptr->d.cdr) {
                (np++)->val=argptr->d.car;
                TNP;
            }

	    if(TYPE(a->bcd.discipline) == STRNG)
	      vtemp = Ifcall(a);	/* foreign function */
	    else
              vtemp = (*(lispval (*)())(a->bcd.entry))(); /* go for it */
            break;

        case ARRAY:
            vtemp = Iarray(a,argptr,FALSE);
            break;


        case DTPR:
            if (a->d.car==nlambda || a->d.car==macro) {
                (np++)->val = argptr;
                TNP;
            } else if (a->d.car==lambda)
                for (; argptr!=nil; argptr = argptr->d.cdr) {
                    (np++)->val = argptr->d.car;
                    TNP;
                }
            else if(a->d.car==lexpr) {
                for (; argptr!=nil; argptr = argptr->d.cdr) {
                    
                    (np++)->val = argptr->d.car;
                    TNP;
                }
                handy = newdot();
                handy->d.car = (lispval)lbot;
                handy->d.cdr = (lispval)np;
                PUSHDOWN(lexpr_atom,handy);
                lbot = np;
                (np++)->val = inewint(((lispval *)handy->d.cdr) - (lispval *)handy->d.car);

            } else break;    /* something is wrong - this isn't a proper function */
            rebind(a->d.cdr->d.car,lbot);
            np = lbot;
            for (handy = a->d.cdr->d.cdr;
                handy != nil;
                handy = handy->d.cdr) {
                    vtemp = eval(handy->d.car);    /* go for it */
                }
            }
        if (vtemp != CNIL)
				/* if we get here with a believable value, */
				/* we must have executed a function. */
            {
            popnames(oldbnp);

            /* in case some clown trashed t */

            tatom->a.clb = (lispval) tatom;
            return(vtemp);
            }
        popnames(oldbnp);
        a = (lispval) errorh(Verundef,"apply: Undefined Function ",
					      nil,TRUE,0,oldlbot->val);
    }
    /*NOT REACHED*/
}


/*
 * Rebind -- rebind formal names
 */
rebind(argptr,workp)
register lispval argptr;        /* argptr points to list of atoms */
register struct argent * workp;        /* workp points to position on stack
                       where evaluated args begin */
{
    register lispval vtemp;
    register struct nament *namptr = bnp;
    register struct argent *lbot;
    register struct argent *np;

    for(;argptr != (lispval)nil;
         workp++,argptr = argptr->d.cdr)  /* rebind formal names (shallow) */
    {
        if(argptr->d.car==nil)
            continue;
        namptr->atm = argptr->d.car;
        if (workp < np) {
            namptr->val = namptr->atm->a.clb;
            namptr->atm->a.clb = workp->val;
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

/* the argument to Lfuncal is optional, if it is given  then it is 
 * the name of the function to call and lbot points to the first arg.
 * if it is not given, then lbot points to the function to call
 */
lispval
Lfuncal(fcn)
lispval fcn;
{
    register lispval a;
    register lispval handy; 
    register struct nament **namptr;
    struct nament *oldbnp = bnp;	/* MUST be first local for evalframe */
    lispval fcncalled;
    lispval Ifcall(),Llist(),Iarray();
    lispval vtemp;
    int typ, dopopframe = FALSE;
    extern lispval end[];
    Savestack(3);

    if(nargs()==1)			/* function I am evaling.    */
	a = fcncalled = fcn;
    else {
	a = fcncalled = lbot->val;
	lbot++;
    }

    /*debugging 
    if (rsetsw && rsetatom->a.clb != nil) {
	printf("funcall:");
	printr(a,stdout);
	printf("\nrsetsw: %d evalhsw: %d\n", rsetsw, evalhsw);
	printf("*rset: ");
	printr(rsetatom->a.clb,stdout);
	printf(" funhook: ");
	printr(funhatom->a.clb,stdout);
	printf(" funhook call flag^G: %d\n",funhcallsw);
	fflush(stdout); 
    };  
    */

    /* check if exception pending */
    if(sigintcnt > 0 ) sigcall(SIGINT);

    if (rsetsw && rsetatom->a.clb != nil)  /* if (*rset t) has been done */
    {    	
	if (evalhsw != nil && funhatom->a.clb != nil)
	{
						/*if (sstatus evalhook t)
						    and evalhook non-nil */
	    if (funhcallsw)
			/*if this is a call to evalhook, don't call evalhook
			  function, but clear evalhcallsw , so recursive 
			  calls to eval cause evalhook function to fire. */
		funhcallsw = FALSE;
	    else {
		/* setup equivalent of (funcall funcallhook <args to eval>) */
		protect(a);
		a = fcncalled = funhatom->a.clb; /* new function to funcall */
		PUSHDOWN(funhatom, nil);	/* lambda-bind 
						 * funcallhook to nil
						 */
		PUSHDOWN(evalhatom, nil);	
	     /* printf(" now will funcall ");
		printr(a,stdout);
		putchar('\n');
		fflush(stdout); */
	    };
	}
	errp = Pushframe(F_FUNCALL,a);
	dopopframe = TRUE;	/* remember to pop later */
	if(retval == C_FRETURN)
	{
	    Restorestack();
	    popnames(oldbnp);
	    errp = Popframe();
	    return(lispretval);
	}
    };

    for(EVER)
    {
     top:
        typ = TYPE(a);
        if (typ == ATOM) a = a->a.fnbnd, typ = TYPE(a);

            /* get function defn (unless calling form */
            /* is itself a lambda-expr) */
        vtemp = CNIL-1;            /* sentinel value for error test */
        switch (typ) {
        case ARRAY:
	    protect(a);			/* stack array descriptor on top */
	    a = a->ar.accfun;		/* now funcall access function */
	    goto top;
        case BCD:
            if(a->bcd.discipline==nlambda)
                {   if(np==lbot) protect(nil);  /* default is nil */
                while(np-lbot!=1 || (lbot->val != nil &&
                      TYPE(lbot->val)!=DTPR)) {

			    lbot->val = errorh(Vermisc,"Bad funcall arg(s) to fexpr.",
						 nil,TRUE,0,lbot->val);
			    
                    np = lbot+1;
                    }
                }
            /* go for it */

            if(TYPE(a->bcd.discipline)==STRNG)
                vtemp = Ifcall(a);
            else
                vtemp = (*(lispval (*)())(a->bcd.entry))();
            if(a->bcd.discipline==macro)
                vtemp = eval(vtemp);
            break;


        case DTPR:
            if (a->d.car == lambda) {
                ;/* VOID */
            } else if (a->d.car == nlambda || a->d.car==macro) {
                if( np==lbot ) protect(nil);    /* default */
                while(np-lbot!=1 || (lbot->val != nil &&
                          TYPE(lbot->val)!=DTPR)) {
                    lbot->val = error("Bad funcall arg(s) to fexpr.",TRUE);
                    np = lbot+1;
                    }
            } else if (a->d.car == lexpr) {
                handy = newdot();
                handy->d.car = (lispval) lbot;
                handy->d.cdr = (lispval) np;
                PUSHDOWN(lexpr_atom,handy);
                lbot = np;
                (np++)->val = inewint(((lispval *)handy->d.cdr) - (lispval *)handy->d.car);
            } else break;        /* something is wrong - this isn't a proper function */
            rebind(a->d.cdr->d.car,lbot);
            np = lbot;
            for (handy = a->d.cdr->d.cdr;
                handy != nil;
                handy = handy->d.cdr) {
                    vtemp = eval(handy->d.car);    /* go for it */
                }
            if(a->d.car==macro)
                vtemp = eval(vtemp);
        }
        if (vtemp != CNIL-1)
            /* if we get here with a believable value, */
            /* we must have executed a function. */
            {
            popnames(oldbnp);

            /* in case some clown trashed t */

            tatom->a.clb = (lispval) tatom;

	    if(dopopframe) errp = Popframe();
            return(vtemp);
            }
        popnames(oldbnp);
	    a = fcncalled = (lispval) errorh(Verundef,"funcall: Bad function",
					       nil,TRUE,0,fcncalled);
    }
    /*NOT REACHED*/
}

/* The following must be the next "function" after Lfuncal, for the
sake of Levalf.  */
fchack () {}

#undef protect

/* protect **************************************************************/
/* pushes the first argument onto namestack, thereby protecting from gc */
lispval
protect(a)
lispval a;
{
    (np++)->val = a;
       if (np >=  nplim)
        namerr();
}

/* unprot ****************************************************************/
/* returns the top thing on the name stack.  Underflow had better not    */
/* occur.                                */
lispval
unprot()
    {
    return((--np)->val);
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
    return(errorh(Verundef,"Undefined function called from compiled code ",
				      nil,TRUE,0,atmn));
}
bindfix(firstarg)
lispval firstarg;
{
    register lispval *argp = &firstarg;
    register struct nament *mybnp = bnp;
    while(*argp != nil) {
        mybnp->atm = *argp++;
        mybnp->val = mybnp->atm->a.clb;
        mybnp->atm->a.clb = *argp++;
        bnp = mybnp++;
    }
}
