#ifndef lint
static char *sccsid = "%W% %G%";
#endif

#include "global.h"
#include "frame.h"

lispval
Levalf ()
{
    register struct frame *myfp;
    register lispval handy, result;
    struct frame *searchforpdl();
    int evaltype;
    Savestack(3);

    if(lbot==np) handy = nil;
    else if((np-lbot) == 1) handy = lbot->val;
    else argerr("evalf");

    if (handy == nil)  /* Arg of nil means start at the top */
    {
	myfp = searchforpdl(errp);	
	/* 
	 * myfp may be nil, if *rset t wasn't done. In that case we
	 * just return nil
	 */
	if(myfp == (struct frame *) 0) return(nil);
	/*
	 * myfp may point to the call to evalframe, in which case we
	 * want to go to the next frame down.  myfp will not point
	 * to the call to evalframe if for example the translink tables
	 * are turned on and the call came from compiled code
	 */
	if(    ((myfp->class == F_EVAL) 
	             && TYPE(myfp->larg1) == DTPR
	             && myfp->larg1->d.car == Vevalframe)
	    || ((myfp->class == F_FUNCALL)
		     && (myfp->larg1 = Vevalframe)))
	    
	   myfp = searchforpdl(myfp->olderrp);	/* advance to next frame */
    } 
    else 
    {
	if( TYPE(handy) != INT ) 
	    error("Arg to evalframe must be integer",TRUE);
	    /* 
	     * Interesting artifact: A pdl pointer will be an INT, but if
	     * read in, the Franz reader produces a bignum, thus giving some
	     * protection from being hacked. 
	     */
	    
	myfp = (struct frame *)(handy->i);
	vfypdlp(myfp);	/* make sure we are given valid pointer */
	myfp = searchforpdl(myfp);
        if (myfp == (struct frame *) 0 ) return(nil);	/* end of frames */
	myfp = searchforpdl(myfp->olderrp);	/* advance to next one */
    };


    if (myfp == (struct frame *) 0 ) return(nil);	/* end of frames */

    if(myfp->class == F_EVAL) evaltype = TRUE; else evaltype = FALSE;

    /* return ( <eval or apply> <fp> <exp being evaled> <bnp>) */
    protect(result = newdot());
    /* 
     * See maclisp manual for difference between eval frames and apply
     * frames, or else see the code below. 
     */
    result->d.car = matom (evaltype ? "eval" : "apply"); 
    result->d.cdr = (handy = newdot());
    handy->d.car = inewint(myfp); /* The frame pointer as a lisp int */
    handy->d.cdr = newdot();
    handy = handy->d.cdr;
    if (evaltype)
	handy->d.car = myfp->larg1;  /* eval type - simply the arg to eval */
    else 
    {  /*  
        * apply type ; must build argument list. The form will look like
	*
 	*	  (<function> (<evaled arg1> <evaled arg2> ....))
	*   i.e. the function name followed by a list of evaluated args 
	*/
	lispval form, arglist;
	struct argent *pntr;
	(form = newdot())->d.car = myfp->larg1;
	handy->d.car = form;			/* link in to save from gc */
	(form->d.cdr = newdot())->d.cdr = nil;
	for (arglist = nil, pntr = myfp->svlbot; pntr < myfp->svnp;  pntr++) 
	{
	    if(arglist == nil) 
	    {
		protect(arglist = newdot());
		form->d.cdr->d.car = arglist;	/* save from gc */
	    }
	    else arglist = (arglist->d.cdr = newdot());
	    arglist->d.car = pntr->val;
	};
    };
    handy->d.cdr = newdot();
    handy = handy->d.cdr;
	/* Next is index into bindstack lisp pseudo-array, for maximum
	    usefulness */
    handy->d.car = inewint( myfp->svbnp - orgbnp); 
    handy->d.cdr = newdot();
    handy = handy->d.cdr;
	
    handy->d.car = inewint(myfp->svnp - orgnp);	/* index of np in namestack*/
    handy->d.cdr = newdot();
    handy = handy->d.cdr;
    handy->d.car = inewint(myfp->svlbot - orgnp);/* index of lbot in namestack*/
    Restorestack();
    return(result);
}

struct frame *searchforpdl (myfp)
struct frame *myfp;
{
    /*
     * for safety sake, we verify that this is a real pdl pointer by
     * tracing back all pdl pointers from the start
     * then after we find it, we just advance to next F_EVAL or F_FUNCALL
     */
    vfypdlp(myfp);
    for(  ; myfp != (struct frame *)0 ; myfp= myfp->olderrp)
    {
	if((myfp->class == F_EVAL) || (myfp->class == F_FUNCALL))
	    return(myfp);
    }
    return((struct frame *)0);
}

/*
 * vfypdlp :: verify pdl pointer as existing,  do not return unless
 * it is valid
 */
vfypdlp(curfp)
register struct frame *curfp;
{
    register struct frame *myfp;

    for (myfp = errp; myfp != (struct frame *)0 ; myfp = myfp->olderrp)
	if(myfp == curfp) return;
    errorh1(Vermisc,"Invalid pdl pointer given: ",nil,FALSE,0,inewint(curfp));
}

lispval
Lfretn ()
{
    struct frame *myfp;	
    chkarg(2,"freturn");

    if( TYPE(lbot->val) != INT )
	error("freturn: 1st arg not pdl pointer",FALSE);

    myfp = (struct frame *) (lbot->val->i);
    vfypdlp(myfp);		/* make sure pdlp is valid */

    retval = C_FRETURN;		/* signal coming from freturn */
    lispretval = (lbot+1)->val;	/* value to return 	*/
    Iretfromfr(myfp);
    /* NOT REACHED */
}
