#ifndef lint
static char *rcsid =
   "$Header: lam5.c,v 1.8 87/12/14 18:47:45 sklower Exp $";
#endif

/*					-[Fri Aug  5 12:49:06 1983 by jkf]-
 * 	lam5.c				$Locker:  $
 * lambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"
#include "chkrtab.h"
#include <ctype.h>
char *strcpy();

/*===========================================
-
-	explode functions: aexplode , aexplodec, aexploden
- The following function partially implement the explode functions for atoms.
-  The full explode functions are written in lisp and call these for atom args.
-
-===========================================*/

#include "chars.h"
lispval
Lexpldx(kind,slashify)
int kind, slashify; 	/* kind = 0 => explode to characters 
				= 1 => explode to fixnums (aexploden)
			   slashify = 0 => do not quote bizarre characters
				    = 1 => quote bizarre characters
			*/
{
	int typ, i;
	char ch, *strb, strbb[BUFSIZ], *alloca();  /* temporary string buffer */
	register lispval last, handy;
	extern int uctolc;
	register char *cp;
	Savestack(3); /* kludge register save mask */
#ifdef SPISFP
	Keepxs();
#endif

	chkarg(1,"expldx");

	handy = Vreadtable->a.clb;
	chkrtab(handy);
	handy = lbot->val;
	*strbuf = 0;
	typ=TYPE(handy);	/* we only work for a few types */


	/* put the characters to return in the string buffer strb */

	switch(typ) {
	case STRNG:
		if(slashify && !Xsdc)
		    errorh1(Vermisc,"Can't explode without string delimiter",nil
					  ,FALSE,0,handy);
		
		strb = strbb;
		if(slashify) *strb++ = Xsdc;
		/* copy string into buffer, escape only occurances of the 
		   double quoting character if in slashify mode
		*/
		for(cp = (char *) handy; *cp; cp++)
		{
		  if(slashify &&
		     (*cp == Xsdc || synclass(ctable[*cp])==CESC))
			 *strb++ = Xesc;
		  *strb++ = *cp;
		}
		if(slashify) *strb++ = Xsdc;
		*strb = NULL_CHAR ;
		strb = strbb;
		break;

	case ATOM:
		strb = handy->a.pname;
		if(slashify && (strb[0]==0)) {
			strb = strbb;
			strbb[0] = Xdqc;
			strbb[1] = Xdqc;
			strbb[2] = 0;
		} else
	/*common:*/
		if(slashify != 0)
		{
			char *out = strbb;
			unsigned char code;

			cp = strb;
			strb = strbb;
			code = ctable[(*cp)&0177];
			switch(synclass(code)) {
			case CNUM:
				*out++ = Xesc;
				break;
			case CCHAR:
				if(uctolc && isupper((*cp)&0177)) {
				    *out++ = Xesc;
				}
				break;
			default:
			    switch(code&QUTMASK) {
			    case QWNUNIQ:
				    if (cp[1]==0) *out++ = Xesc;
				    break;
			    case QALWAYS:
			    case QWNFRST:
				    *out++ = Xesc;
			    }
			}
			*out++ = *cp++;
			for(; *cp; cp++)
			{
				if(((ctable[*cp]&QUTMASK)==QALWAYS) ||
				   (uctolc && isupper(*cp)))
					*out++ = Xesc;
				*out++ = *cp;
			}
			*out = 0;
		}
		break;
				
	case INT:
		strb = strbb;
		sprintf(strb, "%d", lbot->val->i);
		break;
	case DOUB:
		strb = strbb;
		lfltpr(strb, lbot->val->r);
		break;
	case SDOT:
	{
		struct _iobuf _strbuf;
		int count;
		for((handy = lbot->val), count = 12;
		    handy->s.CDR!=(lispval) 0;
		    (handy = handy->s.CDR), count += 12);
		strb = alloca(count);

		_strbuf._flag = _IOWRT+_IOSTRG;
		_strbuf._ptr = strb;
		_strbuf._cnt = count;
		pbignum(lbot->val,&_strbuf);
		putc(0,&_strbuf);
		break;
	}
	default:
			errorh1(Vermisc,"EXPLODE ARG MUST BE STRING, SYMBOL, FIXNUM, OR FLONUM",nil,FALSE,0,handy);
			Restorestack();
			Freexs();
			return(nil);
		}


	if( strb[0] != NULL_CHAR )	/* if there is something to do */
	{
	    lispval prev;

	    protect(handy = last = newdot()); 
	    strbuf[1] = NULL_CHAR ;     /* set up for getatom */
	    atmlen = 2;

	    for(i=0; ch = strb[i++]; ) {
		switch(kind) {

		  case 0: strbuf[0] = hash = ch;   /* character explode */
			  last->d.car = (lispval) getatom(TRUE); /* look in oblist */
			  break;

		  case 1: 
			  last->d.car = inewint(ch);
			  break;
		}

		/* advance pointers */
		prev = last;
		last->d.cdr = newdot();
		last = last->d.cdr;
	    }

	    /* end list with a nil pointer */
	    prev->d.cdr = nil;
	    Freexs();
	    Restorestack();
	    return(handy);
	}
	Freexs();
	Restorestack();
	return(nil);	/* return nil if no characters */
}

/*===========================
-
- (aexplodec 'atm) returns (a t m)
- (aexplodec 234) returns (\2 \3 \4)
-===========================*/

lispval
Lxpldc()
{ return(Lexpldx(0,0)); }


/*===========================
-
- (aexploden 'abc) returns (65 66 67)
- (aexploden 123)  returns (49 50 51)
-=============================*/


lispval
Lxpldn()
{ return(Lexpldx(1,0)); }

/*===========================
-
- (aexplode "123")  returns (\\ \1 \2 \3);
- (aexplode 123)  returns (\1 \2 \3);
-=============================*/

lispval
Lxplda()
{ return(Lexpldx(0,1)); }

/*
 * (argv) returns how many arguments where on the command line which invoked
 * lisp; (argv i) returns the i'th argument made into an atom;
 */

lispval
Largv()
{
	register lispval handy;
	extern int Xargc;
	extern char **Xargv;

	if(lbot-np==0)handy = nil;
	else handy = lbot->val;
	
	if(TYPE(handy)==INT && handy->i>=0 && handy->i<Xargc) {
		strcpy(strbuf,Xargv[handy->i]);
		return(getatom(FALSE));
	} else { 
		return(inewint(Xargc));
	}
}
/*
 * (chdir <atom>) executes a chdir command
 * if successful, return t otherwise returns nil
 */
lispval Lchdir(){
	register char *filenm;

	chkarg(1,"chdir");
	filenm = (char *) verify(lbot->val,"chdir - non symbol or string arg");
	if(chdir(filenm)>=0)
		return(tatom);
	else
		return(nil);
}

/* ==========================================================
-
-	ascii   - convert from number to ascii character
-
- form:(ascii number)
-
-	the number is checked so that it is in the range 0-255
- then it is made a character and returned
- =========================================================*/

lispval
Lascii() 
{
	register lispval handy;

	handy = lbot->val;		/* get argument */

	if(TYPE(handy) != INT)		/* insure that it is an integer */
	{	error("argument not an integer",FALSE);
		return(nil);
	}

	if(handy->i < 0 || handy->i > 0377)	/* insure that it is in range*/
	{	error("argument is out of ascii range",FALSE);
		return(nil);
	}

	strbuf[0] = handy->i ;	/* ok value, make into a char */
	strbuf[1] = NULL_CHAR;

	/* lookup and possibly intern the atom given in strbuf */

	return( (lispval) getatom(TRUE) );
}

/*
 *  boole - maclisp bitwise boolean function
 *  (boole k x y) where k determines which of 16 possible bitwise 
 *  truth tables may be applied.  Common values are 1 (and) 6 (xor) 7 (or)
 *  the result is mapped over each pair of bits on input
 */
lispval
Lboole(){
	register x, y;
	register struct argent *mynp;
	int k;

	if(np - lbot < 3)
		error("Boole demands at least 3 args",FALSE);
	mynp = lbot+AD;
	k = mynp->val->i & 15;
	x = (mynp+1)->val->i;
	for(mynp += 2; mynp < np; mynp++) {
		y = mynp->val->i;
		switch(k) {

		case 0: x = 0;
			break;
		case 1: x = x & y;
			break;
		case 2:	x = y & ~x;
			break;
		case 3:	x = y;
			break;
		case 4: x = x & ~y;
			break;
		/* case 5:	x = x; break; */
		case 6:	x = x ^ y;
			break;
		case 7:	x = x | y;
			break;
		case 8: x = ~(x | y);
			break;
		case 9: x = ~(x ^ y);
			break;
		case 10: x = ~x;
			break;
		case 11: x = ~x | y;
			break;
		case 12: x = ~y;
			break;
		case 13: x = x | ~y;
			break;
		case 14: x = ~x | ~y;
			break;
		case 15: x = -1;
		}
	}
	return(inewint(x));
}
lispval
Lfact()
{
	register lispval result, handy;
	register itemp;
	Savestack(3); /* fixup entry mask */

	result = lbot->val;
	if(TYPE(result)!=INT) error("Factorial of Non-fixnum.  If you want me\
to calculate fact of > 2^30 We will be here till doomsday!.",FALSE);
	itemp = result->i;
	protect(result = newsdot());
	result->s.CDR=(lispval)0;
	result->i = 1;
	for(; itemp > 1; itemp--)
		dmlad(result,(long)itemp,0L);
	if(result->s.CDR) 
	{
	    Restorestack();
	    return(result);
	}
	handy = inewint(result->s.I);
	pruneb(result);
	Restorestack();
	return(handy);
}
/*
 * fix -- maclisp floating to fixnum conversion
 * for the moment, mereley convert floats to ints.
 * eventual convert to bignum if too big to fit.
 */
 lispval Lfix() 
 {
	register lispval handy;
	double floor();

	chkarg(1,"fix");
	handy = lbot->val;
	switch(TYPE(handy)) {
	default:
		error("innaproriate arg to fix.",FALSE);
	case INT:
	case SDOT:
		return(handy);
	case DOUB:
		return(inewint((int)floor(handy->r)));
	}
}
/*
 * (frexp <real no>)
 * returns a dotted pair (<exponent>. <bignum>)
 * such that bignum is 56 bits long, and if you think of the binary
 * point occuring after the high order bit, <real no> = 2^<exp> * <bignum>
 *
 * myfrexp is an assembly language routine found in bigmath.s to do exactly
 * what is necessary to accomplish this.
 * this routine is horribly vax specific.
 *
 * Lfix should probably be rewritten to take advantage of myfrexp
 */
lispval
Lfrexp()
{
	register lispval handy, result;
	int exp, hi, lo;

	Savestack(2);
	chkarg(1,"frexp");

	myfrexp(lbot->val->r, &exp, &hi, &lo);
	if(lo < 0) {
		/* normalize for bignum */
		lo &= ~ 0xC0000000;
		hi += 1;
	}
	result = handy = newdot(); 
	protect(handy);
	handy->d.car = inewint(exp);
	if(hi==0&&lo==0) {
		handy->d.cdr = inewint(0);
	} else {
		handy = handy->d.cdr = newsdot();
		handy->s.I = lo;
		handy = handy->s.CDR = newdot();
		handy->s.I = hi;
		handy->s.CDR = 0;
	}
	np--;
	Restorestack();
	return(result);
}

#define SIGFPE 8
#define B 1073741824.0
static double table[] = { 1.0, B, B*B, B*B*B, B*B*B*B, 0.0};

lispval
Lfloat()
{
	register lispval handy,result;
	register double sum = 0;
	register int count;
	chkarg(1,"float");
	handy = lbot->val;
	switch(TYPE(handy))
	{
	  case DOUB: return(handy);


	  case INT:  result = newdoub();
		     result->r = (double) handy->i;
		     return(result);
	  case SDOT: 
	  {
		for(handy = lbot->val, count = 0;
		    count < 5;
		    count++, handy = handy->s.CDR) {
			sum += handy->s.I * table[count];
			if(handy->s.CDR==(lispval)0) goto done;
		}
		kill(getpid(),SIGFPE);
	done:
		result = newdoub();
		result->r = sum;
		return(result);
	}
	  default: errorh1(Vermisc,"Bad argument to float",nil,FALSE,0,handy);
	  /* NOTREACHED */
	}
}
double
Ifloat(handy)
register lispval handy;
{
	register double sum = 0.0; register int count=0;
	for(; count < 5; count++, handy = handy->s.CDR) {
		sum += handy->s.I * table[count];
		if(handy->s.CDR==(lispval)0) goto done;
	}
	kill(getpid(),SIGFPE);
	done:
	return(sum);
}

/* Lbreak ***************************************************************/
/* If first argument is not nil, this is evaluated and printed.  Then	*/
/* error is called with the "breaking" message.				*/
lispval Lbreak() {

	if (np > lbot) {
		printr(lbot->val,poport);
		dmpport(poport);
	}
	return(error("",TRUE));
}


lispval
LDivide() {
	register lispval result, work;
	register struct argent *mynp;
	lispval quo, rem, arg1, arg2; struct sdot dummy, dum2;
	Savestack(3);

	chkarg(2,"Divide");
	mynp = lbot;
	work = mynp++->val;
	switch(TYPE(work)) {
	case INT:
		arg1 = (lispval) &dummy;
		dummy.I = work->i;
		dummy.CDR = (lispval) 0;
		break;
	case SDOT:
		arg1 = work;
		break;
	urk:
	default:
		error("First arg to divide neither a bignum nor int.",FALSE);
	}
	work = mynp->val;
	switch(TYPE(work)) {
	case INT:
		arg2 = (lispval) &dum2;
		dum2.I = work->i;
		dum2.CDR = (lispval) 0;
		break;
	case SDOT:
		arg2 = work;
		break;
	default:
		goto urk;
	}
	divbig(arg1,arg2, &quo, &rem);
	protect(quo);
	if(rem==((lispval)&dummy))
		rem = inewint(dummy.I);
	protect(rem);
	protect(result = work = newdot());
	work->d.car = quo;
	(work->d.cdr = newdot())->d.car = rem;
	Restorestack();
	return(result);
}

lispval LEmuldiv(){
	register struct argent * mynp = lbot+AD;
	register lispval work, result;
	int quo, rem;
	Savestack(3); /* fix register mask */

	/* (Emuldiv mul1 mult2 add quo) => 
		temp = mul1 + mul2 + sext(add);
		result = (list temp/quo temp%quo);
		to mix C and lisp a bit */

	Imuldiv(mynp[0].val->i, mynp[1].val->i, mynp[2].val->i,
		mynp[3].val->i, &quo, &rem);
	protect(result=newdot());
	(result->d.car=inewint(quo));
	work = result->d.cdr = newdot();
	(work->d.car=inewint(rem));
	Restorestack();
	return(result);
}
