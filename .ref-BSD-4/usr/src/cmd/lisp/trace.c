static char *sccsid = "@(#)trace.c	34.1 10/3/80";

#include "global.h"
lispval
Leval1(){
    register struct nament *bindptr;
    register lispval handy;
    snpand(2);
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
    snpand(1);
    chkarg(2,"evalhook");
    if (evalhsw == 0) 
	    error("evalhook called before doing sstatus-evalhook", TRUE);
    if (rsetsw == 0 || rsetatom->a.clb == nil)
	    error("evalhook called while not in *rset mode", TRUE);
    PUSHDOWN(evalhatom,(lispval)(lbot+1)->val);
    /* eval checks evalhcall to see if this is a LISP call to evalhook
	in which case it avoids call to evalhook function, but clobbers
	value to nil so recursive calls will check.  */
    PUSHDOWN(evalhcall,tatom);
    handy = eval(lbot->val);
    POP;
    POP;
    return(handy);
}

lispval
Lrset ()
    {
    chkarg(1,"rset");

    rsetsw = (lbot->val == nil) ? 0 : 1;
    rsetatom->a.clb = (lbot->val == nil) ? nil: tatom;
    return(lbot->val);
}

