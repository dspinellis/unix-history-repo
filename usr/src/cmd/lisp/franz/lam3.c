
static char *sccsid = "@(#)lam3.c	35.4 7/8/81";

# include "global.h"
lispval
Lalfalp()
{
	register char  *first, *second;

	chkarg(2,"alphalessp");
	first = (char *) verify(lbot->val,"alphalessp: non symbol or string arg");
	second = (char *) verify((lbot+1)->val,"alphalessp: non symbol or string arg");
	if(strcmp(first,second) < 0)
		return(tatom);
	else
		return(nil);
}

lispval
Lncons()
{
	register lispval handy;

	chkarg(1,"ncons");
	handy = newdot();
	handy->d.cdr = nil;
	handy->d.car = lbot->val;
	return(handy);
}
lispval
Lzerop()
{
	register lispval handy;

	chkarg(1,"zerop");
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
	register lispval handy; 
	lispval Ladd();

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
	struct argent *onp = np;
	Savestack(4);


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
		if(Lnegp()==nil) 
		{
		    Restorestack();
		    return(nil);
		}
	}
	Restorestack();
	return(tatom);
}

lispval
Lgreaterp()
{
	register int typ;
	/* do the easy cases first */
	if(np-lbot == 2)
	{   if((typ=TYPE(lbot->val)) == INT)
	    {    if((typ=TYPE(lbot[1].val)) == INT)
		   return((lbot[0].val->i - lbot[1].val->i) > 0 ? tatom : nil);
		 else if(typ == DOUB)
		  return((lbot[0].val->i - lbot[1].val->r) > 0.0 ? tatom : nil);
	    }
	    else if(typ == DOUB)
	    {    if((typ=TYPE(lbot[1].val)) == INT)
		  return((lbot[0].val->r - lbot[1].val->i) > 0.0 ? tatom : nil);
		 else if(typ == DOUB)
		  return((lbot[0].val->r - lbot[1].val->r) > 0.0 ? tatom : nil);
	    }
	}
		  
	return(cmpx(FALSE));
}

lispval
Llessp()
{
	register int typ;
	/* do the easy cases first */
	if(np-lbot == 2)
	{   if((typ=TYPE(lbot->val)) == INT)
	    {    if((typ=TYPE(lbot[1].val)) == INT)
		   return((lbot[0].val->i - lbot[1].val->i) < 0 ? tatom : nil);
		 else if(typ == DOUB)
		  return((lbot[0].val->i - lbot[1].val->r) < 0.0 ? tatom : nil);
	    }
	    else if(typ == DOUB)
	    {    if((typ=TYPE(lbot[1].val)) == INT)
		  return((lbot[0].val->r - lbot[1].val->i) < 0.0 ? tatom : nil);
		 else if(typ == DOUB)
		  return((lbot[0].val->r - lbot[1].val->r) < 0.0 ? tatom : nil);
	    }
	}
		  
	return(cmpx(TRUE));
}

lispval
Ldiff()
{
	register lispval arg1,arg2; 
	register handy = 0;


	chkarg(2,"Ldiff");
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
	register lispval arg1,arg2;
	lispval  handy;
	struct sdot fake1, fake2;
	fake2.CDR = 0;
	fake1.CDR = 0;

	chkarg(2,"mod");
	handy = arg1 = lbot->val;
	arg2 = (lbot+1)->val;
	switch(TYPE(arg1)) {
	case SDOT:
		switch(TYPE(arg2)) {
		case SDOT:			/* both are already bignums */
			break;
		case INT:			/* convert arg2 to bignum   */
			fake2.I = arg2->i;
			arg2 =(lispval) &fake2;
			break;
		default:
			error("non-numeric argument",FALSE);
		}
		break;
	case INT:
		switch(TYPE(arg2)) {
		case SDOT:			/* convert arg1 to bignum */
			fake1.I = arg1->i;
			arg1 =(lispval) &fake1;
			break;
		case INT:			/* both are fixnums 	  */
			return( inewint ((arg1->i) % (arg2->i)) );
		default:
			error("non-numeric argument",FALSE);
		}
		break;
	default:
		error("non-numeric argument",FALSE);
	}
	if(TYPE((lbot+1)->val)==INT && lbot[1].val->i==0)
		return(handy);
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
	Savestack(1); /* fixup entry mask */

	handy = rdrint;
	handy->i = 1;
	protect(handy);
	handy=Ladd();
	Restorestack();
	return(handy);

}

lispval
Lsub1()
{
	register lispval handy;
	lispval Ladd();
	Savestack(1); /* fixup entry mask */

	handy = rdrint;
	handy->i = - 1;
	protect(handy);
	handy=Ladd();
	Restorestack();
	return(handy);
}

lispval
Lminus()
{
	register lispval arg1, handy;
	register temp;
	lispval subbig();

	chkarg(1,"minus");
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
	case SDOT: { struct sdot dummyb;
		handy = (lispval) &dummyb;
		handy->s.I = 0;
		handy->s.CDR = (lispval) 0;
		handy = subbig(handy,arg1);
		break; }

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

loop:
	switch(TYPE(handy)) {
	case INT:
		if(handy->i < 0) flag = TRUE;
		break;
	case DOUB:
		if(handy->r < 0) flag = TRUE;
		break;
	case SDOT:
		for(work = handy;
		    work->s.CDR!=(lispval) 0;
		    work = work->s.CDR) {;}
		if(work->s.I < 0) flag = TRUE;
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

	chkarg(1,"absval");
	arg1 = lbot->val;
	if(Lnegp()!=nil) return(Lminus());

	return(arg1);
}

#include "vaxframe.h"
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
	lispval linterp();
	lispval _qfuncl(),tynames();	/* locations in qfuncl */
	extern int prinlevel,prinlength;

	if(TYPE(Vprinlevel->a.clb) == INT)
	{ 
	   prinlevel = Vprinlevel->a.clb->i;
	}
	else prinlevel = -1;
	if(TYPE(Vprinlength->a.clb) == INT)
	{
	    prinlength = Vprinlength->a.clb->i;
	}
	else prinlength = -1;

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
		(myfp->pc > Lfuncal &&		/* compiled code */
		 myfp->pc < linterp)  )
	    {
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
 *
 * (oblist)
 *
 * oblist returns a list of all symbols in the oblist
 *
 * written by jkf.
 */
lispval
Loblist()
{
    int indx;
    lispval headp, tailp ;
    struct atom *symb ;
    extern int hashtop;
    Savestack(0);

    headp = tailp = newdot(); /* allocate first DTPR */
    protect(headp);		/*protect the list from garbage collection*/
				/*line added by kls			  */

    for( indx=0 ; indx <= hashtop-1 ; indx++ ) /* though oblist */
    {
	for( symb = hasht[indx] ;
	     symb != (struct atom *) CNIL ;
	     symb = symb-> hshlnk)
	{
	    if(TYPE(symb) != ATOM) 
	    {   printf(" non symbol in hasht[%d] = %x: ",indx,symb);
		printr(symb,stdout);
		printf(" \n");
		fflush(stdout);
	    }
	    tailp->d.car = (lispval) symb  ; /* remember this atom */
	    tailp = tailp->d.cdr = newdot() ; /* link to next DTPR */
	}
    }

    tailp->d.cdr = nil ; /* close the list unfortunately throwing away
			  the last DTPR
			  */
    Restorestack();
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
#include "chars.h"
#include "chkrtab.h"

lispval
Lsetsyn()
{
	register lispval s, c;
	register struct argent *mynp;
	register index;
	lispval x,debugmode;
	extern unsigned char *ctable;
	extern lispval Istsrch();
	int value;

	switch(np-lbot) {
	case 2:
		x= nil;			/* only 2 args given */
	case 3:
		x = lbot[2].val;	/* all three args given */
		break;
	default:
		argerr("setsyntax");
	}
	s = Vreadtable->a.clb;
	chkrtab(s);
	/* debugging code 
	debugmode = Istsrch(matom("debugging"))->d.cdr->d.cdr->d.cdr;
	if(debugmode)  printf("Readtable addr: %x\n",ctable);
	  end debugging code */
	mynp = lbot;
	c = (mynp++)->val;
	s = (mynp++)->val;

	switch(TYPE(c)) {
	default:
		error("neither fixnum, atom or string as char to setsyntax",FALSE);

	case ATOM:
		index = *(c->a.pname);
		if((c->a.pname)[1])error("Only 1 char atoms to setsyntax",FALSE);
		break;

	case INT:
		index = c->i;
		break;

	case STRNG:
		index = (int) *((char *) c);
	}
	switch(TYPE(s)) {
	case INT:
		if(s->i == VESC) Xesc = (char) index;
		else if(s->i == VDQ) Xdqc = (char) index;
		else if(s->i == VSD) Xsdc = (char) index;	/* string */

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
		else if(ctable[index] == VSD  /* and for string delimiter */
			&& s->i != VSD
			&& Xsdc == (char) index) {
			 ctable[index] = s->i;
			 rpltab(VSD,&Xsdc);
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
		Iputprop(c,x,lastrtab);
	}
	return(tatom);
}

/*
 * this aux function is used by setsyntax to determine the new current
 * escape or double quote character.  It scans the character table for
 * the first character with the given class (either VESC or VDQ) and
 * puts that character in Xesc or Xdqc (whichever is pointed to by
 * addr).
 */
rpltab(cclass,addr)
char cclass;
char *addr;
{
	register int i;
	extern unsigned char *ctable;
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
