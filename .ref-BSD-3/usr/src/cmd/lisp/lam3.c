# include "global.h"
lispval
Lalfalp()
{
	register lispval first, second;
	register struct argent *inp;
	snpand(3); /* clobber save mask */

	chkarg(2);
	inp = lbot;
	first = (inp)->val;
	second = (inp+1)->val;
	if( (TYPE(first))!=ATOM || (TYPE(second))!=ATOM)
		error("alphalessp expects atoms");
	if(strcmp(first->pname,second->pname) <= 0)
		return(tatom);
	else
		return(nil);
}

lispval
Lncons()
{
	register lispval handy;
	snpand(1); /* clobber save mask */

	chkarg(1);
	handy = newdot();
	handy -> cdr = nil;
	handy -> car = lbot->val;
	return(handy);
}
lispval
Lzerop()
{
	register lispval handy;
	snpand(1); /* clobber save mask */

	chkarg(1);
	handy = lbot->val;
	switch(TYPE(handy)) {
	case INT:
		return(handy->i==0?tatom:nil);
	case DOUB:
		return(handy->r==0.0?tatom:nil);
	}
	return(nil);
}
lispval
Lonep()
{
	register lispval handy; lispval Ladd();
	snpand(1); /* clobber save mask */

	chkarg(1);
	handy = lbot->val;
	switch(TYPE(handy)) {
	case INT:
		return(handy->i==1?tatom:nil);
	case DOUB:
		return(handy->r==1.0?tatom:nil);
	case SDOT:
		protect(inewint(0));
		handy = Ladd();
		if(TYPE(handy)!=INT || handy->i !=1)
			return(nil);
		else
			return(tatom);
	}
	return(nil);
}

lispval
cmpx(lssp)
{
	register struct argent *argp;
	register struct argent *outarg;
	register struct argent *handy;
	register count;
	register struct argent *lbot;
	register struct argent *np;
	struct argent *onp = np;


	argp = lbot + 1;
	outarg = np;
	while(argp < onp) {

		np = outarg + 2;
		lbot = outarg;
		if(lssp)
			*outarg = argp[-1], outarg[1]  = *argp++;
		else
			outarg[1]  = argp[-1], *outarg = *argp++;
		lbot->val = Lsub();
		np = lbot + 1;
		if(Lnegp()==nil) return(nil);
	}
	return(tatom);
}

lispval
Lgreaterp()
{
	return(cmpx(FALSE));
}

lispval
Llessp()
{
	return(cmpx(TRUE));
}

lispval
Ldiff()
{
	register lispval arg1,arg2; register handy = 0;
	snpand(3); /* clobber save mask */


	chkarg(2);
	arg1 = lbot->val;
	arg2 = (lbot+1)->val;
	if(TYPE(arg1)==INT && TYPE(arg2)==INT) {
		handy=arg1->i - arg2->i;
	}
	else error("non-numeric argument",FALSE);
	return(inewint(handy));
}

lispval
Lmod()
{
	register lispval arg1,arg2; lispval  handy;
	struct sdot fake1, fake2;
	fake2.CDR = 0;
	fake1.CDR = 0;
	snpand(2); /* clobber save mask */

	chkarg(2);
	handy = arg1 = lbot->val;
	arg2 = (lbot+1)->val;
	switch(TYPE(arg1)) {
	case SDOT:
		break;
	case INT:
		fake1.I = arg1->i;
		arg1 =(lispval) &fake1;
		break;
	default:
		error("non-numeric argument",FALSE);
	}
	switch(TYPE(arg2)) {
	case SDOT:
		break;
	case INT:
		fake2.I = arg2->i;
		arg2 =(lispval) &fake2;
		break;
	default:
		error("non-numeric argument",FALSE);
	}
		if(Lzerop()!=nil) return(handy);
		divbig(arg1,arg2,0,&handy);
		if(handy==((lispval)&fake1))
			handy = inewint(fake1.I);
		if(handy==((lispval)&fake2))
			handy = inewint(fake2.I);
		return(handy);

}


lispval
Ladd1()
{
	register lispval handy;
	lispval Ladd();
	snpand(1); /* fixup entry mask */

	handy = rdrint;
	handy->i = 1;
	protect(handy);
	return(Ladd());

}

lispval
Lsub1()
{
	register lispval handy;
	lispval Ladd();
	snpand(1); /* fixup entry mask */

	handy = rdrint;
	handy->i = - 1;
	protect(handy);
	return(Ladd());
}

lispval
Lminus()
{
	register lispval arg1, handy;
	register temp;
	lispval subbig();
	snpand(3); /* clobber save mask */

	chkarg(1);
	arg1 = lbot->val;
	handy = nil;
	switch(TYPE(arg1)) {
	case INT:
		handy= inewint(0 - arg1->i);
		break;
	case DOUB:
		handy = newdoub();
		handy->r = -arg1->r;
		break;
	case SDOT:
		handy = rdrsdot;
		handy->I = 0;
		handy->CDR = (lispval) 0;
		handy = subbig(handy,arg1);
		break;

	default:
		error("non-numeric argument",FALSE);
	}
	return(handy);
}

lispval
Lnegp()
{
	register lispval handy = np[-1].val, work;
	register flag = 0;
	snpand(3); /* clobber save mask */

loop:
	switch(TYPE(handy)) {
	case INT:
		if(handy->i < 0) flag = TRUE;
		break;
	case DOUB:
		if(handy->r < 0) flag = TRUE;
		break;
	case SDOT:
		for(work = handy; work->CDR!=(lispval) 0; work = work->CDR);
		if(work->I < 0) flag = TRUE;
		break;
	default:
		handy = errorh(Vermisc,
				  "minusp: Non-(int,real,bignum) arg: ",
				  nil,
				  TRUE,
				  0,
				  handy);
		goto loop;
	}
	if(flag) return(tatom);
	return(nil);
}

lispval
Labsval()
{
	register lispval arg1, handy;
	register temp;
	snpand(3); /* clobber save mask */

	chkarg(1);
	arg1 = lbot->val;
	if(Lnegp()!=nil) return(Lminus());

	return(arg1);
}

#include "frame.h"
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
	register struct frame *myfp; register lispval handy;
	int **fp;	/* this must be the first local */
	int virgin=1;
	lispval _qfuncl(),tynames();	/* locations in qfuncl */

	if(f==1)
		printf("Forms in evaluation:\n");
	else
		printf("Backtrace:\n\n");

	myfp = (struct frame *) (&fp +1);	/* point to current frame */

	while(TRUE)
	{
	    if( (myfp->pc > eval  &&  		/* interpreted code */
		 myfp->pc < popnames)
		||
		(myfp->pc > _qfuncl &&		/* compiled code */
		 myfp->pc < tynames)  )
	    {
		handy = (myfp->ap[1]);
		if(f==1)
			printr(handy,stdout), putchar('\n');
		else {
			if(virgin)
				virgin = 0;
			else
				printf(" -- ");
			printr((TYPE(handy)==DTPR)?handy->car:handy,stdout);
		}

	    }

	    if(myfp > myfp->fp) break;	/* end of frames */
	    else myfp = myfp->fp;
	}
	putchar('\n');
	return(nil);
}
lispval
Lbaktrace()
{
	isho(0);
}
/* ===========================================================
-
**** baktrace ****	(moved back by kls)
-
- baktrace will print the names of all functions being evaluated
- from the current one (baktrace) down to the first one.
- currently it only prints the function name.  Planned is a
- list of local variables in all stack frames.
- written by jkf.
-
-============================================================*/

/*=============================================================
-
-***  oblist ****
-
- oblist returns a list of all symbols in the oblist
-
- written by jkf.
============================================================*/

lispval
Loblist()
{
    int indx;
    lispval headp, tailp ;
    struct atom *symb ;

    headp = tailp = newdot(); /* allocate first DTPR */
    protect(headp);		/*protect the list from garbage collection*/
				/*line added by kls			  */

    for( indx=0 ; indx <= HASHTOP-1 ; indx++ ) /* though oblist */
    {
	for( symb = hasht[indx] ;
	     symb != (struct atom *) CNIL ;
	     symb = symb-> hshlnk)
	{
	    tailp->car = (lispval) symb  ; /* remember this atom */
	    tailp = tailp->cdr = newdot() ; /* link to next DTPR */
	}
    }

    tailp->cdr = nil ; /* close the list unfortunately throwing away
			  the last DTPR
			  */
    return(headp);
}

/*
 * Maclisp setsyntax function:
 *    (setsyntax c s x)
 * c represents character either by fixnum or atom
 * s is the atom "macro" or the atom "splicing" (in which case x is the
 * macro to be invoked); or nil (meaning don't change syntax of c); or
 * (well thats enough for now) if s is a fixnum then we modify the bits
 * for c in the readtable.
 */
#define VMAC	0316
#define VSPL	0315
#define VDQ     0212
#define VESC	0217
#include "chkrtab.h"

lispval
Lsetsyn()
{
	register lispval s, c;
	register struct argent *mynp;
	register index;
	register struct argent *lbot, *np;
	lispval x;
	extern char *ctable;
	int value;

	chkarg(3);
	s = Vreadtable->clb;
	chkrtab(s);
	mynp = lbot;
	c = (mynp++)->val;
	s = (mynp++)->val;
	x = (mynp++)->val;

	switch(TYPE(c)) {
	default:
		error("neither fixnum nor atom as char to setsyntax",FALSE);

	case ATOM:
		index = *(c->pname);
		if((c->pname)[1])error("Only 1 char atoms to setsyntax",FALSE);
		break;

	case INT:
		index = c->i;
	}
	switch(TYPE(s)) {
	case INT:
		if(s->i == VESC) Xesc = (char) index;
		else if(s->i == VDQ) Xdqc = (char) index;

		if(ctable[index] == VESC   /* if we changed the current esc */
		  && s->i != VESC          /* to something else, pick current */
		  && Xesc == (char) index) {
	       		ctable[index] = s->i;
			rpltab(VESC,&Xesc);
		}
		else if(ctable[index] == VDQ   /*  likewise for double quote */
		       && s->i != VDQ
		       && Xdqc == (char) index)  {
			ctable[index] = s->i;
			rpltab(VDQ,&Xdqc);
		}
		else ctable[index] = s->i;

		break;
	case ATOM:
		if(s==splice)
			ctable[index] = VSPL;
		else if(s==macro)
			ctable[index] = VMAC;
		if(TYPE(c)!=ATOM) {
			strbuf[0] = index;
			strbuf[1] = 0;
			c = (getatom());
		}
		Iputprop(c,x,macro);
	}
	return(tatom);
}



/* this aux function is used by setsyntax to determine the new current
   escape or double quote character.  It scans the character table for
   the first character with the given class (either VESC or VDQ) and
   puts that character in Xesc or Xdqc (whichever is pointed to by
   addr).
*/
rpltab(cclass,addr)
char cclass;
char *addr;
{
	register int i;
	extern char *ctable;
	for(i=0; i<=127 && ctable[i] != cclass; i++);
	if(i<=127) *addr = (char) i;
	else *addr = '\0';
}



lispval
Lzapline()
{
	register FILE *port;
	extern FILE * rdrport;

	port = rdrport;
	while (!feof(port) && (getc(port)!='\n') );
	return(nil);
}

