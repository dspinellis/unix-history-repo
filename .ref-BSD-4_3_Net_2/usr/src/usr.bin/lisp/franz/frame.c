#ifndef lint
static char *rcsid =
   "$Header: frame.c,v 1.3 87/12/14 16:51:52 sklower Exp $";
#endif

/*
 * 	frame.c				$Locker:  $
 * non local goto handlers
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include "global.h"
#include "frame.h"

/* 
 * This is a collection of routines for manipulating evaluation frames.
 * Such frames are generated to mark the state of execution at a certain
 * spot.  They are created upon entry to prog, do, catch, errset and
 * other misc. functions (such as eval when in *rset mode).
 *
 * As described in h/frame.h, each frame is identified by a class, which
 * says who created the frame.  The global variable errp points to the
 * first (newest) frame on the stack.  
 * The standard way to create a frame is to say
 *
 *   errp = Pushframe(class,arg1,arg2);	 /* create and link in new 
 *   					    frame of give class * /
 *
 * poping the frame must be done explicity if the routine was not exited by
 * a non-local goto.  This is done by 
 *   errp = Popframe();
 *
 * When a frame is created, it marks the current state on the runtime stack.
 * Execution will continues after the Pushframe call with the value of the
 * global variable 'retval' set to 0.  Some time later control may be thrown
 * up the stack and it will seem that Pushframe returned again.  This time
 * retval will contain a non-zero value indicating what caused the non-local
 * jump.  retval will have one of the values from C_???? in h/frame.h . 
 * It will not have just of the C_???? values, it will only have a value
 * which makes sense. For example, coming out of a Pushframe(F_CATCH,tag,nil);
 * retval will either be 0 (initially) or C_THROW, [and in addition it will
 * already have been determined that the tag of the catch matches the tag
 * being thrown, [[ this does not apply to GO's and PROG tags]] ].
 *
 * In doing throws, goto's, returns, or errors up the stack we are always
 * conscious of the possiblity of unwind-protect sitting between where 
 * control starts and where it wants to get.  Thus it may be necessary
 * to save the state of the non-local jump, give control to the unwind-protect
 * and have it continue the non-local jump. 
 */

 /*
  * Inonlocalgo(class, arg1, arg2) :: do a general non-local goto.
  *    	class - one of the C_???? in h/frame.h
  *	arg1 - tag in C_THROW, C_GO; value in C_RETURN
  *	arg2 - value in C_THROW;
  *  this handles GO's, THROW's, RETURN's  but not errors, which have more
  * state to throw and a lot of different things to do if there is no one
  * to catch the error.
  * 
  * This routine never returns.
  */

Inonlocalgo(class, arg1, arg2)
lispval arg1,arg2;
{
    struct frame *uwpframe, *Inlthrow();
    lispval handy;

    /* 
     * scan for something to match 'class', return if nothing found, or
     * if we must first handle an unwind protect.
     */
    while( uwpframe = Inlthrow(class,arg1,arg2) )
    {
	/* build error frame description to be use to continue this throw */
	protect(lispretval = handy = newdot());
    	handy->d.car = Veruwpt;
	handy = handy->d.cdr = newdot();
	handy->d.car = inewint(class);		/* remember type */
	handy = handy->d.cdr = newdot();
	handy->d.car = arg1;
	handy = handy->d.cdr = newdot();
	handy->d.car = arg2;
	retval = C_THROW;
	Iretfromfr(uwpframe);
	/* NOT REACHED */
    }

    /*
     * nothing to go to, signal the appropriate error
     */

    switch(class)
    {
    case C_GO: errorh1(Vermisc, "No prog to go to with this tag ",
			       nil,FALSE,0,arg1);
		/* NOT REACHED */

    case C_RET: errorh(Vermisc, "No prog to return from", nil, FALSE, 0);
		/* NOT REACHED */

    case C_THROW: errorh1(Vermisc, "No catch for this tag ", nil, FALSE , 0,
				  arg1);
		/* NOT REACHED */
    default: error("Internal  Inonlocalgoto error" ,FALSE);
		/* NOT REACHED */
    }
}

/*
 * Inlthrow(class,arg1,arg2) :: look up the stack for a form to handle
 * a value of 'class' being thrown.  If found, do the throw.  If an
 * unwind-protect must be done, then return a pointer to that frame
 * first.  If there is nothing to catch this throw, we return 0.
 */

struct frame *
Inlthrow(class, arg1, arg2)
lispval arg1, arg2;
{
    struct frame *uwpframe = (struct frame *)0;
    struct frame *curp;
    int pass = 1;

    restart:
	for(curp = errp; curp != (struct frame *) 0; curp = curp->olderrp)
	{
	    switch(curp->class)
	    {
	    case F_PROG: if(class == C_RET || class == C_GO)
			 {
			    if(pass == 2) return(uwpframe);
			    else 
			    {
				lispretval = arg1;
				retval = class;
				Iretfromfr(curp);
				/* NOT REACHED */
			    }
			  }
			  break;

	    case F_CATCH: if((pass == 1) && (curp->larg1 == Veruwpt))
			  {
				uwpframe = curp;
				pass = 2;
				goto restart;
			  }
			  else if(class == C_THROW 
					&& matchtags(arg1,curp->larg1))
			  {
			    if(pass == 2) return(uwpframe);
			    else 
			    {
				lispretval = arg2;	/* value thrown */
				retval = class;
				Iretfromfr(curp);
				/* NOT REACHED */
			    }
			   }
			   break;
	    
	    case F_RESET:  if(class == C_RESET)
			   {
				if(pass == 2) return(uwpframe);
				else
				{
				    retval = class;
				    Iretfromfr(curp);
				    /* NOT REACHED */
				}
			    }
			    break;

	    }
	}
	return((struct frame *)0);   /* nobody wants it */
}


#ifndef tahoe
Iretfromfr(fram)
register struct frame *fram;
{
    xpopnames(fram->svbnp);
    qretfromfr();	/* modified in sed script to point to real function */
    /* NOT REACHED */
}
#endif

/* matchtags :: return TRUE if there is any atom in common between the
 * two tags.  Either tag may be an atom or an list of atoms.
 */
matchtags(tag1,tag2)
lispval tag1, tag2;
{
    int repeat1 = FALSE;
    int repeat2 = FALSE;
    lispval temp1 = tag1;
    lispval temp2 = tag2;
    lispval t1,t2;

    if(TYPE(tag1) == ATOM) 
    {
	t1 = tag1;
    }
    else {
	t1 = tag1->d.car;
	repeat1 = TRUE;
    }

    if(TYPE(tag2) == ATOM)
    {
	t2 = tag2;
    }
    else {
	t2 = tag2->d.car;
	repeat2 = TRUE;
    }

loop:
    if(t1 == t2) return(TRUE);
    if(repeat2) 
    {
	if((temp2 = temp2->d.cdr) != nil)
	{
	    t2 = temp2->d.car;
	    goto loop;
	}
    }

    if(repeat1)
    {
        if((temp1 = temp1->d.cdr) != nil)
	{
	    t1 = temp1->d.car;
	    if(repeat2) 
	    {
	        temp2 = tag2;
		t2 = temp2->d.car;
		goto loop;
	    }
	    else t2 = tag2;
	    goto loop;
	 }
    }
    return(FALSE);
}

/*
 * framedump :: debugging routine to print the contents of the error 
 * frame
 *
 */
lispval
Lframedump()
{
    struct frame *curp;

    printf("Frame dump\n");
    for(curp = errp ; curp != (struct frame *)0 ; curp=curp->olderrp)
    {
	printf("at %x is ",curp);

	switch(curp->class) {
	case F_PROG: printf(" prog\n");
		     break;

	case F_CATCH:printf(" catching ");
		     printr(curp->larg1,stdout);
		     putchar('\n');
		     break;

	case F_RESET:printf(" reset \n");
		     break;

	case F_EVAL: printf(" eval: ");
		     printr(curp->larg1,stdout);
		     putchar('\n');
		     break;

	case F_FUNCALL: printf(" funcall: ");
		     printr(curp->larg1,stdout);
		     putchar('\n');
		     break;

	case F_TO_FORT: printf(" calling fortran:\n");
		     break;

	case F_TO_LISP: printf(" fortran calling lisp:\n");
		     break;

		
	default:
		     printf(" unknown: %d \n",curp->class);
	}
	fflush(stdout);
    }
    printf("End of stack\n");
    return(nil);
}

