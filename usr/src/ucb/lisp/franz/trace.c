#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/trace.c,v 1.2 83/08/19 09:50:34 jkf Exp $";
#endif

/*					-[Thu Aug 18 10:08:36 1983 by jkf]-
 * 	trace.c				$Locker:  $
 * evalhook evaluator
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"
lispval
Leval1(){
    register struct nament *bindptr;
    register lispval handy;
    if (np-lbot == 2) {	/*if two arguments to eval */
	if (TYPE((lbot+1)->val) != INT)
	    error("Eval: 2nd arg not legal alist pointer", FALSE);
	bindptr = orgbnp + (lbot+1)->val->i;
	if (rsetsw == 0 || rsetatom->a.clb == nil)
	    error("Not in *rsetmode; second arg is useless - eval", TRUE);
	if (bptr_atom->a.clb != nil)
	    error("WARNING - Nesting 2nd args to eval will give spurious values", TRUE);
	if (bindptr < orgbnp || bindptr >bnplim)
	    error("Illegal pdl pointer as 2nd arg - eval", FALSE);
	handy = newdot();
	handy->d.car = (lispval)bindptr;
	handy->d.cdr = (lispval)bnp;
	PUSHDOWN(bptr_atom, handy); 
	handy = eval(lbot->val);
	POP;
	return(handy);
    } else {	/* normal case - only one arg */
	chkarg(1,"eval");
	handy = eval(lbot->val);
	return(handy);
    };
}

lispval
Levalhook()
{
    register lispval handy;
    register lispval funhval = CNIL;

    switch (np-lbot) 
    {
    case 2: break;
    case 3: funhval = (lbot+2)->val;
	    break;
    default: argerr("evalhook");
    }

    /* Don't do this check any longer
     * if (evalhsw == 0) 
     *	    error("evalhook called before doing sstatus-evalhook", TRUE);
     * if (rsetsw == 0 || rsetatom->a.clb == nil)
     *    error("evalhook called while not in *rset mode", TRUE);
     */
     
    if(funhval != CNIL) { PUSHDOWN(funhatom,funhval); }

    PUSHDOWN(evalhatom,(lispval)(lbot+1)->val);
    /* eval checks evalhcall to see if this is a LISP call to evalhook
	in which case it avoids call to evalhook function, but clobbers
	value to nil so recursive calls will check.  */
    evalhcallsw = TRUE;	
    handy = eval(lbot->val);
    POP;

    if(funhval != CNIL) { POP; }

    return(handy);
}


lispval
Lfunhook()
{
    register lispval handy;
    register lispval evalhval = CNIL;
    Savestack(2);


    switch (np-lbot) 
    {
    case 2: break;
    case 3: evalhval = (lbot+2)->val;
	    break;
    default: argerr("funcallhook");
    }

    /* Don't do this check any longer
     * if (evalhsw == 0) 
     *	    error("funcallhook called before doing sstatus-evalhook", TRUE);
     *if (rsetsw == 0 || rsetatom->a.clb == nil)
     *	    error("funcallhook called while not in *rset mode", TRUE);
     */
     
    handy = lbot->val;
    while (TYPE(handy) != DTPR) 
      handy = errorh1(Vermisc,"funcallhook: first arg must be a list",nil,TRUE,
					   0,handy);
    if(evalhval != CNIL) { PUSHDOWN(evalhatom,evalhval); }

    PUSHDOWN(funhatom,(lispval)(lbot+1)->val);
    /* funcall checks funcallhcall to see if this is a LISP call to evalhook
	in which case it avoids call to evalhook function, but clobbers
	value to nil so recursive calls will check.  */
    funhcallsw = TRUE;	
    /*
     * the first argument to funhook is a list of already evaluated expressions
     * which we just stack can call funcall on
     */
    lbot = np;		/* base of new args */
    for ( ; handy != nil ; handy = handy->d.cdr)
    {
	protect(handy->d.car);
    }
    handy = Lfuncal();
    POP;
    if(evalhval != CNIL) { POP;  }
    Restorestack();
    return(handy);
}


lispval
Lrset ()
    {
    chkarg(1,"rset");

    rsetsw = (lbot->val == nil) ? 0 : 1;
    rsetatom->a.clb = (lbot->val == nil) ? nil: tatom;
    evalhcallsw = FALSE;
    return(lbot->val);
}

