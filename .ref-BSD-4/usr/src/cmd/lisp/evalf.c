static char *sccsid = "@(#)evalf.c	34.1 10/3/80";

#include "global.h"
#include "frame.h"
/* evalframe off of the c stack.  
    We will set fp to point where the register fp points.
    Then fp+2 = saved ap
         fp+4 = saved pc
         fp+3 = saved fp
         ap+1 = first arg
*/

/* These will keep track of the current saved values of np and lbot
as we decend the evalstack.  These must be read by decoding the registers saved
by each function call. */
struct argent *fakenp;
struct argent *fakelbot;

lispval
Levalf ()
{
    register struct frame *myfp;
    struct frame *nextevf();
    register lispval handy, result;
    int **fp;	/* this must be the first local */
    int evaltype;
    snpand(3);
    if(lbot==np) {
	protect (nil);
    };
    chkarg(1,"evalf");
    fakenp = NULL;
    fakelbot = NULL;
    if (lbot->val == nil) { /* Arg of nil means start at the top */
	myfp = nextevf ((struct frame *) (&fp +1), &evaltype);	
		    /* myfp  now points to evalframe of call to evalframe */
	myfp = myfp->fp; /* and now to the one past evalframe */
    } else {
	if( TYPE(lbot->val) != INT )
	    /* Interesting artifact: A pdl pointer will be an INT, but if
	    read in, the Franz reader produces a bignum, thus giving some
	    protection from being hacked.  */
	    error("ARG TO EVALFRAME MUST BE INTEGER",TRUE);
	myfp = (struct frame *) (lbot->val->i);
	if (myfp < (struct frame *) (&fp +1)){
		/* if purported fp is less than current fp, a fraud
		(since stack grows down, and current fp must be bottom) */
	    error("ARG TO EVALFRAME NOT EVALFRAME POINTER", TRUE);
	}
    };
    myfp = nextevf(myfp, &evaltype);  /* get pointer to frame above */
    if(myfp > myfp->fp) return(nil);	/* end of frames */
	/* return ( <eval or apply> <fp> <exp being evaled> <bnp>) */
    protect(result = newdot());
	/* See maclisp manual for difference between eval frames and apply
	frames, or else see the code below. */
    result->d.car = matom (evaltype ? "eval" : "apply"); 
    result->d.cdr = (handy = newdot());
    handy->d.car = inewint(myfp->fp); /* The frame pointer as a lisp int */
    handy->d.cdr = newdot();
    handy = handy->d.cdr;
    if (evaltype)
	handy->d.car = myfp->ap[1];  /* eval type - simply the arg to eval */
    else {  /*  apply type ; must build argument list. The form will look like
		  (<function> (<evaled arg1> <evaled arg2> ....))
	      i.e. the function name followed by a list of evaluated args */
	lispval form, handy1, arglist;
	struct argent *pntr;
	    /* name of function will either be arg to Lfuncal or on argstack */
	(form = newdot())->d.car = 
	    (int)myfp->ap[0] & 1? myfp->ap[1] : (fakelbot-1)->val; 
			/* Assume that Lfuncal increments lbot after getting
			  function to call. */
	(form->d.cdr = newdot())->d.cdr = nil;
	for (arglist = nil, pntr = fakenp;
	     pntr > fakelbot;) {
	    (handy1 = newdot())->d.cdr = arglist;
	    (arglist = handy1)->d.car = (--pntr)->val;
	};
	form->d.cdr->d.car = arglist;
	handy->d.car = form;
    };
    handy->d.cdr = newdot();
    handy = handy->d.cdr;
	/* Next is index into bindstack lisp pseudo-array, for maximum
	    usefulness */
    handy->d.car = inewint( ((struct nament *) *( ((long *)myfp->fp) -1))
			 -orgbnp); /* first part gets oldbnp, if first local */
    handy->d.cdr = newdot();
    handy = handy->d.cdr;
	
    handy->d.car = inewint(fakenp-orgnp);   /* index of np in namestack*/
    handy->d.cdr = newdot();
    handy = handy->d.cdr;
    handy->d.car = inewint(fakelbot-orgnp); /* index of lbot in namestack*/
    return(result);
}

#define LBOTNPMASK 03<<22  /* Octal 3 */
/* We assume that r6 and r7 are saved as pairs, and that no earlier
registers are saved.  If the Franz snpand hack is changed, this may
have to change too. */
struct frame *nextevf (curfp, ftypep)
struct frame *curfp;
int *ftypep;
{
    register struct frame *myfp;
    lispval _qfuncl(),tynames();	/* locations in qfuncl */
    lispval fchack(); 			/*pseudo function after Lfuncal */
    for (myfp = curfp; myfp < myfp->fp; myfp = myfp->fp) {
		/* Look up stack until find a frame with the right saved pc */
	if (myfp->mask & LBOTNPMASK)  {
	    fakenp = (struct argent *)(myfp->r6);
	    fakelbot = (struct argent *)(myfp->r7);
	    };
	if (myfp->pc > eval && myfp->pc < popnames) {	/* interpreted code */
	    *ftypep = TRUE;
	    break;
	} else 
/*	if (myfp->pc > _qfuncl && myfp->pc < tynames) { /* compiled code *//*
	    *ftypep = FALSE;
	    break;
	} else   */
	if (myfp->pc > Lfuncal && myfp->pc < fchack) { /* call to funcall */
	    *ftypep = FALSE;
	    break;
	};
    };
    return(myfp);
}

#include "catchfram.h"
lispval
Lfretn ()
{
    int **fp;  		/* this must be the first local */
    struct frame *myfp;	
    struct nament *mybnp;
    extern long errp;
    extern long exitlnk;
    typedef struct catchfr *cp;
    typedef struct savblock *savp;
    cp curp;
    savp cursavp;
    chkarg(2,"freturn");
    if( TYPE(lbot->val) != INT )
	error("freturn: 1st arg not pdl pointer",FALSE);
    myfp = (struct frame *) (lbot->val->i);
    if (myfp < (struct frame *) (&fp +1)){
	    /* if purported fp is less than current fp, a fraud
	    (since stack grows down, and current fp must be bottom) */
	error("freturn: 1st arg not current pdl pointer", FALSE);
    };
	/* Unwind name stack.  The oldbnp will be the first local variable of
	the function we are returning from, so it will be immediately below this
	stack frame (i.e. it was pushed right after the call). */
    mybnp = (struct nament *) *(((long *)myfp) - 1);
    if (mybnp < orgbnp || mybnp > bnp) 
	error("freturn: problem with pdl pointer", FALSE);
    popnames (mybnp);
	/* Reset pointer to next catchframe in stack appropriately. */
    for (curp = (cp) errp ; curp != (cp) nil ; curp = curp->link){
	/* Debugging...
	printf ("Considering catchframe at %d\n", curp); fflush(stdout); */
	if ((long *) myfp < (long *) curp) {
	    /* printf ("Won\n"); fflush(stdout); */
	    break;
	};
    };
    errp = (long)curp;
    /* printf ("errp is now %d\n", errp);fflush(stdout); */
	/* Reset saveblock for setexit/reset appropriately. */
    for (cursavp = (savp)exitlnk; cursavp != (savp) NULL; 
		 cursavp = cursavp->savlnk) {
	/* printf("Considering saveblock at %d\n", cursavp);
	fflush (stdout); */
	if ((savp) myfp > cursavp && 
	    ((savp)myfp < cursavp->savlnk || cursavp->savlnk == 0)) {
	    /* printf("Won\n"); fflush(stdout); */
	    resexit(cursavp);
	    break;
	};
    };
    fsmash(myfp, (np-1)->val);	/* Smash the fp register..(If myfp not valid fp,
			    real trouble follows)  Will really return 
			    from other guy (ha ha) */
}

fsmash(framep, retval)
struct frame *framep;
lispval retval;
{
    asm("	movl	4(ap), fp");
    asm("	movl	8(ap), r0");
    asm("	ret");
}

