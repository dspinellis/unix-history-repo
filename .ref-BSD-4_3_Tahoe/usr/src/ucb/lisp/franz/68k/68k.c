#include "global.h"
#include <signal.h>


mmuladd(a,b,c,m)
long a,b,c,m;
{
	long work[2]; char err;
	emul(a,b,c,work);
	ediv(work,m,err);
	return(work[0]);
}
/*mmuladd (a, b, c, m) 
int a, b, c, m;
{
	asm ("emul	4(ap),8(ap),12(ap),r0");
	asm ("ediv	16(ap),r0,r2,r0");
}

Imuldiv() {
asm("	emul	4(ap),8(ap),12(ap),r0");
asm("	ediv	16(ap),r0,*20(ap),*24(ap)");
}*/

Imuldiv(p1,p2,add,dv,quo,rem)
long p1, p2, add, dv;
long *quo, *rem;
{
	long work[2]; char err;

	emul(p1,p2,add,work);
	*quo = ediv(work,dv, &err);
	*rem = *work;
}
/*C library -- write
  nwritten = write(file, buffer, count);
  nwritten == -1 means error
*/
write(file, buffer, count)
char *buffer;
{
	register lispval handy;
	int retval;
	if((file != 1) || (Vcntlw->a.clb == nil)) goto top;
	/* since ^w is non nil, we do not want to print to the terminal,
	   but we must be sure to return a correct value from the write
	   in case there is no write to ptport
	*/
	retval = count;
	goto skipit;

top:

	retval = _write(file,buffer,count);

skipit:
    if(file==1) {
	handy = Vptport->a.clb;
	if(handy!=nil && TYPE(handy)==PORT && handy->p->_file!=1) {
		fflush(handy->p);
		file = handy->p->_file;
		goto top;
	}
    }
    return(retval);
}

/*
# C library -- read

# nread = read(file, buffer, count);
#
# nread ==0 means eof; nread == -1 means error
*/
#include <errno.h>
read(file,buffer,count)
{
	extern int errno;
	register int Size;
again:

	Size = _read(file,buffer,count);
	if ((Size >= 0) || (errno != EINTR)) return(Size);
	if(sigintcnt > 0) sigcall(SIGINT);
	goto again;
}

lispval
Lpolyev()
{
	register int count; 
	register double *handy, *base;
	register struct argent *argp;
	lispval result; int type;
	char *alloca();

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
/*	asm("polyd	(r9),r11,8(r9)");
	asm("movd	r0,(r9)");*/
	result = newdoub();
	result->r = *base;
	return(result);
}

lispval
Lrot()
{
	register rot,val;		/* these must be the first registers */
	register struct argent *mylbot = lbot;

	chkarg(2,"rot");
	if((TYPE(mylbot->val) != INT) || (TYPE(mylbot[1].val) != INT))
		errorh2(Vermisc,
		       "Non ints to rot",
		       nil,FALSE,0,mylbot->val,mylbot[1].val);
	val = mylbot[0].val->i;
	rot = mylbot[1].val->i;
	rot = rot % 32 ;	/* bring it down below one byte in size */
	if(rot < 0) {
		rot = -rot;
		{asm("roll	d7,d6");}
	} else {asm("rorl	d7,d6");}
	return( inewint(val));
}

myfrexp() { error("myfrexp called", FALSE);}
#if os_unisoft | os_unix_ts
syscall() { error("vsyscall called", FALSE);}
#endif

#include "structs.h"
prunei(what)
register lispval what;
{
	extern struct types int_str;
	int gstart();
	if(((long)what) > ((long) gstart)) {
		--(int_items->i);
		what->i = (long) int_str.next_free;
		int_str.next_free = (char *) what;
	}
}
#include "68kframe.h"
/* new version of showstack,
	We will set fp to point where the register fp points.
	If we find that the saved pc is somewhere in the routine eval,
   then we print the first argument to that eval frame. This is done
   by looking on the stack.
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
	lispval linterp(), Ifuncal();
	lispval _qfuncl(),tynames();	/* locations in qfuncl */
	extern int plevel,plength;

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

	myfp = (struct machframe *) (&fp +1);	/* point to current machframe */

	while(TRUE)
	{
	    if( (myfp->pc > eval  &&  		/* interpreted code */
		 myfp->pc < popnames)
		||
		(myfp->pc > Ifuncal &&		/* compiled code */
		 myfp->pc < Lfuncal)  )
	    {
	      { handy = (myfp->fp->ap[0]);
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
 * very vax specific
 */
lispval
LIshowstack()
{
    int **fp;	/* must be the first local variable */
    register lispval handy;
    register struct machframe *myfp;
    lispval retval, Ifuncal();
    Savestack(2);
    
    chkarg(1,"int:showstack");

    if((TYPE(handy=lbot[0].val) != INT) && (handy != nil))
        error("int:showstack non fixnum arg", FALSE);

    if(handy == nil)
        myfp = (struct machframe *) (&fp +1);
    else
        myfp = (struct machframe *) handy->i;
	
    if((int ***)myfp <= &fp) error("int:showstack illegal stack value",FALSE);
    while(myfp > 0)
    {
        if( (myfp->pc > eval  &&  		/* interpreted code */
            myfp->pc < popnames)
	    ||
	    (myfp->pc > Ifuncal &&		/* compiled code */
	    myfp->pc < Lfuncal)  )
        {
	    {
		handy = (lispval)(myfp->fp->ap[0]);	/* arg to eval */

		protect(retval=newdot());
		retval->d.car = handy;
		if(myfp > myfp->fp)
		    myfp = 0;	/* end of frames */
		else
		    myfp = myfp->fp;
		retval->d.cdr = inewint(myfp);
		return(retval);
	    }
	}
	if(myfp > myfp->fp)
	     myfp = 0;	/* end of frames */
	else
	     myfp = myfp->fp;

    }
    return(nil);
}
#include "frame.h"
/*
 * this code is very similar to ftolsp.
 * if it gets revised, so should this.
 */
lispval
dothunk(func,count)
lispval func;
long count;
{
	register long *arglist = (& count) + 3;
	lispval save;
	pbuf pb;
	Savestack(1);

	if(errp->class==F_TO_FORT)
		np = errp->svnp;
	errp = Pushframe(F_TO_LISP,nil,nil);
	lbot = np;
	np++->val = func;
	for(; count > 0; count--)
		np++->val = inewint(*arglist++);
	save = Lfuncal();
	errp = Popframe();
	Restorestack();
	return(save);
}
/*
_thcpy:
	movl	sp@,a0
	movl	a0@+,sp@-
	movl	a0@+,sp@-
	jsr	_dothunk
	lea	sp@(12),sp
	rts*/
static char fivewords[] = "01234567890123456789";

lispval
Lmkcth()
{
	register struct argent *mylbot = lbot;
	register struct thunk {
		short	nop;
		short 	jsri;
		char	*thcpy;
		long	count;
		lispval func;
	} *th;
	long handy = (long) pinewstr(fivewords);
	extern char thcpy[];

	chkarg(2,"make-c-thunk");
	handy = ((handy - 1 ) | 3) + 1;
	th = (struct thunk *) handy;
	th->nop = 0x4e71;
	th->jsri = 0x4eb9;
	th->thcpy = thcpy;
	th->func = mylbot->val;
	th->count = mylbot[1].val->i;

	return((lispval)th);
}
