/*
 * 	tahoe.c
 * tahoe specific functions
 *
 * (c) copyright 1982, Regents of the University of California
 */
 
#include "global.h"
#include <signal.h>

mmuladd (a, b, c, m) 
int a, b, c, m;
{
	asm ("emul	4(fp),8(fp),12(fp),r0");
	asm ("ediv	16(fp),r0,r2,r0");
}

Imuldiv(a, b, c, d, e)
{
	asm("	emul	4(fp),8(fp),12(fp),r0");
	asm("	ediv	16(fp),r0,*20(fp),*24(fp)");
}

lispval
Lpolyev()
{
	register int count; 
	register double *handy, *base;
	register struct argent *argp;
	lispval result; int type;
	char *alloca();
	Keepxs();

	error("Lpolyev - Unimplemented or inappropriate CCI function",FALSE);
	count = 2 * (((int) np) - (int) lbot);
	if(count == 0) 
		return(inewint(0));
	if(count == 8)
		return(lbot->val);
	base = handy = (double *) alloca(count);
	for(argp = lbot; argp < np; argp++) {
		while((type = TYPE(argp->val))!=DOUB && type!=INT)
			argp->val = (lispval) errorh2(Vermisc,"%%machine-polyev:non-real arg",nil,TRUE,73,lbot,argp->val);
		if(TYPE(argp->val)==INT) {
			*handy++ = argp->val->i;
		} else
			*handy++ = argp->val->r;
	}
	count = count/sizeof(double) - 2;
#ifdef vax
	asm("polyd	(r9),r11,8(r9)");
	asm("movd	r0,(r9)");
#endif
	result = newdoub();
	result->r = *base;
	Freexs();
	return(result);
}

lispval
Lrot()
{
	register val;
	register unsigned long mask2 = -1;
	register struct argent *mylbot = lbot;
	long rot;

	chkarg(2,"rot");
	if((TYPE(mylbot->val) != INT) || (TYPE(mylbot[1].val) != INT))
		errorh2(Vermisc,
		       "Non ints to rot",
		       nil,FALSE,0,mylbot->val,mylbot[1].val);
	val = mylbot[0].val->i;
	rot = mylbot[1].val->i;
	rot = rot & 0x3f;	/* bring it down below one byte in size */
	mask2 >>=  rot;
	mask2 ^= -1;
	mask2 &= val;
	mask2 >>= (32 - rot);
	val <<= rot;
	val |= mask2;
	return( inewint(val));
}

#include "tahoeframe.h"
/* new version of showstack,
	We will set fp to point where the register fp points.
	Then fp+2 = saved ap
	     fp+4 = saved pc
	     fp+3 = saved fp
	     ap+1 = first arg
	If we find that the saved pc is somewhere in the routine eval,
   then we print the first argument to that eval frame. This is done
   by looking one beyond the saved ap.
*/
lispval
Lshostk()
{	lispval isho();
	return(isho(1));
}
static lispval
isho(f)
int f;
{
	register struct machframe *myfp; register lispval handy;
	int **fp;	/* this must be the first local */
	int virgin=1;
	lispval linterp();
	lispval _qfuncl(),tynames();	/* locations in qfuncl */
	extern int plevel,plength;

	error("C coded showstack - Unimplemented or inappropriate CCI function",FALSE);
	if(TYPE(Vprinlevel->a.clb) == INT)
	{ 
	   plevel = Vprinlevel->a.clb->i;
	}
	else plevel = -1;
	if(TYPE(Vprinlength->a.clb) == INT)
	{
	    plength = Vprinlength->a.clb->i;
	}
	else plength = -1;

	if(f==1)
		printf("Forms in evaluation:\n");
	else
		printf("Backtrace:\n\n");

	myfp = (struct machframe *) (&fp +1);	/* point to current frame */

	while(TRUE)
	{
	    if( (myfp->pc > eval  &&  		/* interpreted code */
		 myfp->pc < popnames)
		||
		(myfp->pc > Lfuncal &&		/* compiled code */
		 myfp->pc < linterp)  )
	    {
#ifdef vax
	      if(((int) myfp->ap[0]) == 1)		/* only if arg given */
	      { handy = (myfp->ap[1]);
		if(f==1)
			printr(handy,stdout), putchar('\n');
		else {
			if(virgin)
				virgin = 0;
			else
				printf(" -- ");
			printr((TYPE(handy)==DTPR)?handy->d.car:handy,stdout);
		}
	      }
#endif

	    }

	    if(myfp > myfp->fp) break;	/* end of frames */
	    else myfp = myfp->fp;
	}
	putchar('\n');
	return(nil);
}

/*
 *
 *	(baktrace)
 *
 * baktrace will print the names of all functions being evaluated
 * from the current one (baktrace) down to the first one.
 * currently it only prints the function name.  Planned is a
 * list of local variables in all stack frames.
 * written by jkf.
 *
 */
lispval
Lbaktrace()
{
	isho(0);
}

/*
 * (int:showstack 'stack_pointer)
 * return
 *   nil if at the end of the stack or illegal
 *   ( expresssion . next_stack_pointer) otherwise
 *   where expression is something passed to eval
 * very tahoe specific
 */


lispval
LIshowstack()
{
    int **fp;	/* must be the first local variable */
    register lispval handy;
    register struct machframe *myfp;
    lispval retval, Lfuncal(), Ifuncal();
    lispval (*pc)() = 0;
    Savestack(2);
    
    chkarg(1,"int:showstack");

    if((TYPE(handy=lbot[0].val) != INT) && (handy != nil))
        error("int:showstack non fixnum arg", FALSE);

    if(handy == nil)
        asm("movab	-8(fp),r11");		/* only way I could think of */
    else
        myfp = (struct machframe *) handy->i;
	
/* if((int ***)myfp <= &fp) error("int:showstack illegal stack value",FALSE); */

    while(myfp > 0)
    {
        /*fprintf(stderr, "myfp=%x pc=%x fp=%x removed=%d\n", myfp, myfp->pc,
			myfp->fp, myfp->removed);
	fflush(stderr);*/

	if( (pc >= eval  &&  		/* interpreted code */
            pc < popnames)
	    ||
	    (pc >= Ifuncal &&		/* compiled code */
	    pc < Lfuncal)  )
        {
	    if(myfp->removed == 8)	/* only if arg given */
	    {
		handy = (lispval)(myfp->arg[0]);	/* arg to eval */

		protect(retval=newdot());
		retval->d.car = handy;
		if(myfp > myfp->fp)
		    myfp = 0;	/* end of frames */
		else
	            myfp = (struct machframe *) ((char *)myfp->fp - 8);
		retval->d.cdr = inewint(myfp);
		return(retval);
	    }
	}
	if(myfp > myfp->fp)
	     myfp = 0;	/* end of frames */
	else
	  {pc = myfp->pc;
	   myfp = (struct machframe *) ((char *)myfp->fp - 8);
	  }
    }
    return(nil);
}

#include "frame.h"
/*
 * this code is very similar to ftolsp.
 * if it gets revised, so should this.
 */
lispval
dothunk(func,count,arglist)
lispval func;
long count;
register long *arglist;
{
	lispval save;
	pbuf pb;

	if(errp->class==F_TO_FORT)
		np = errp->svnp;
	errp = Pushframe(F_TO_LISP,nil,nil);
	lbot = np;
	np++->val = func;
	arglist++; /* this is a vaxism, we'll compensate elsewhere */
	for(; count > 0; count--)
		np++->val = inewint(*arglist++);
	save = Lfuncal();
	errp = Popframe();
	return(save);
}


/*
_thcpy:
	movl	(sp),r0
	pushl	ap
	pushl	(r0)+
	pushl	(r0)+
	calls	$3,_dothunk
	ret */

/*
 * This is thunkmodel:
	.word	0
	movl	r0,r0
	callf	$4,_thunkstack1
	.long	0 <count>
	.long	0 <func>
 */

extern lispval thunkstack1();
struct thunk {
	short	mask;
	char	nop[3];
	char 	callf[3];
	lispval	(*stack1)();
	long	count;
	lispval func;
} thunkmodel =
{ 0, { 0xd , 0x50 , 0x50}, {0xfe , 0x4 , 0x9f}, thunkstack1, 0, 0};
static char sixwords[] = "01234567890123456789012"; /* trailing 0! */

lispval
Lmkcth()
{
	register struct argent *mylbot = lbot;
	register struct thunk *th;


	chkarg(2,"make-c-thunk");
	th = (struct thunk *)pinewstr(sixwords);
	th = (struct thunk *) ((((int) th) | 3) & ~3);
	*th = thunkmodel;
	th->func = mylbot->val;
	th->count = mylbot[1].val->i;

	return((lispval)th);
}

/*
 * This removes the frame from the stack for the thunk
 * and retrieves various data.  (Actually merges it into
 * its own stack frame).
 */
lispval
thunkstack1(retfromthunk)
{
	register int *handy, *midthunk;
	int *arglist;
	lispval func;
	int count;

	handy = &retfromthunk;
	arglist = handy + 2;		/* should be +3, first is taken as
					   vax arglist count and ignored */
	handy[-1] = handy[2];		/* unlink frame */
	midthunk = (int *) handy[-3];	/* our oldpc points to mid thunk */
	handy[-3] = retfromthunk;
	handy[-2] += (8 + handy[1]);	/* save mask for thunk is 0,
					   adjust bytes to remove from us  */

	count = *midthunk;
	func = (lispval) midthunk[1];
	/* you could even merge this in and avoid another callf! */
	return(dothunk(func,count,arglist));
}
