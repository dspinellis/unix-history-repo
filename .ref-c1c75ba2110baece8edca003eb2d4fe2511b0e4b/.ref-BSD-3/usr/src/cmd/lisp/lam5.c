#include "global.h"
#include "chkrtab.h"

/*===========================================
-
-	explode functions
- The following function partially implement two explode functions,
- explodec and exploden.  They only work for atom arguments.
-
-===========================================*/

#include "chars.h"
lispval
Lexpldx(kind,slashify)
int kind, slashify; 	/* 0=explodec   1=exploden  */
{
	int typ, i;
	char ch, *strb, strbb[BUFSIZ];  /* temporary string buffer */
	register lispval last, handy;
	char Idqc = Xdqc;
	snpand(4); /* kludge register save mask */

	chkarg(1);

	handy = Vreadtable->clb;
	chkrtab(handy);
	handy = lbot->val;
	*strbuf = 0;
	typ=TYPE(handy);	/* we only work for a few types */


	/* put the characters to return in the string buffer strb */

	switch(typ) {
	case STRNG:
		strb = (char *) handy;
		if(Xsdc)Idqc = Xsdc;
		goto common;
	case ATOM:
		strb = handy->pname;
		if(strb[0]==0) {
			strb = strbb;
			strbb[0] = Xdqc;
			strbb[1] = Xdqc;
			strbb[2] = 0;
		} else
	common:
		if(slashify != 0)
		{
			register char *cp, *out = strbb;
			cp = strb;
			strb = strbb;
			if(ctable[(*cp)&0177]==VNUM)
				*out++ = Xesc;
			for(; *cp; cp++)
			{
				if(ctable[*cp]& QUTMASK)
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
		sprintf(strb, "%0.16G", lbot->val->r);
		break;
	case SDOT:
	{
		struct _iobuf _strbuf;
		register count;
		for((handy = lbot->val), count = 12;
		    handy->CDR!=(lispval) 0;
		    (handy = handy->CDR), count += 12);
		strb = (char *) alloca(count);

		_strbuf._flag = _IOWRT+_IOSTRG;
		_strbuf._ptr = strb;
		_strbuf._cnt = count;
		pbignum(lbot->val,&_strbuf);
		putc('.',&_strbuf);
		putc(0,&_strbuf);
		break;
	}
	default:
			errorh(Vermisc,"EXPLODE ARG MUST BE STRING, SYMBOL, FIXNUM, OR FLONUM",nil,FALSE,0,handy);
			return(nil);
		}


	if( strb[0] != NULL_CHAR )	/* if there is something to do */
	{
	    register lispval prev;

	    protect(handy = last = newdot()); 
	    strbuf[1] = NULL_CHAR ;     /* set up for getatom */
	    atmlen = 2;

	    for(i=0; ch = strb[i++]; ) {
		switch(kind) {

		  case 0: strbuf[0] = hash = ch;   /* character explode */
			  hash = 177 & hash;	/* cut 1st bit off if any */
			  last->car = (lispval) getatom(); /* look in oblist */
			  break;

		  case 1: 
			  last->car = inewint(ch);
			  break;
		}

		/* advance pointers */
		prev = last;
		last->cdr = newdot();
		last = last->cdr;
	    }

	    /* end list with a nil pointer */
	    prev->cdr = nil;
	    return(handy);
	}
	else return(nil);	/* return nil if no characters */
}

/*===========================
-
- (explodec 'atm) returns (a t m)
- (explodec 234) returns (\2 \3 \4)
-===========================*/

lispval
Lexpldc()
{ return(Lexpldx(0,0)); }


/*===========================
-
- (exploden 'abc) returns (65 66 67)
- (exploden 123)  returns (49 50 51)
-=============================*/


lispval
Lexpldn()
{ return(Lexpldx(1,0)); }

/*===========================
-
- (explodea "123")  returns (\\ \1 \2 \3);
- (explodea 123)  returns (\1 \2 \3);
-=============================*/

lispval
Lexplda()
{ return(Lexpldx(0,1)); }

/*
 * (argv) returns how many arguments where on the command line which invoked
 * lisp; (argv i) returns the i'th argument made into an atom;
 */

lispval
Largv()
{
	register lispval handy;
	register index;
	register char c, *base;
	extern int Xargc;
	extern char **Xargv;

	chkarg(1);
	handy = lbot->val;
	
	if(TYPE(handy)==INT && handy->i>=0 && handy->i<Xargc) {
		strcpy(strbuf,Xargv[handy->i]);
		return(getatom());
	} else { 
		return(inewint(Xargc));
	}
}
/*
 * (chdir <atom>) executes a chdir command
 * if successful, return t otherwise returns nil
 */
lispval Lchdir(){
	register lispval handy;

	chkarg(1);
	handy=lbot->val;
	if(TYPE(handy)==ATOM && (chdir(handy->pname)>=0))
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

	return( (lispval) getatom() );
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
	register lispval result;
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
	snpand(3); /* fixup entry mask */

	result = lbot->val;
	if(TYPE(result)!=INT) error("Factorial of Non-fixnum.  If you want me\
to calculate fact of > 2^30 We will be here till doomsday!.",FALSE);
	itemp = result->i;
	protect(result = newsdot());
	result->CDR=(lispval)0;
	result->i = 1;
	for(; itemp > 1; itemp--)
		dmlad(result,itemp,0);
	if(result->CDR) return(result);
	(handy = newint())->i = result->i;
	return(handy);
}
/*
 * fix -- maclisp floating to fixnum conversion
 * for the moment, mereley convert floats to ints.
 * eventual convert to bignum if too big to fit.
 */
 lispval Lfix() 
 {
	register lispval result, handy;

	chkarg(1);
	handy = lbot->val;
	switch(TYPE(handy)) {
	default:
		error("innaproriate arg to fix.",FALSE);
	case INT:
	case SDOT:
		return(handy);
	case DOUB:
		if(handy->r >= 0)
			return(inewint((int)handy->r));
		else
			return(inewint(((int)handy->r)-1));
	}
}

lispval
Lfloat()
{
	register lispval handy,result;
	chkarg(1);
	handy = lbot->val;
	switch(TYPE(handy))
	{
	  case DOUB: return(handy);


	  case INT:  result = newdoub();
		     result->r = (double) handy->i;
		     return(result);
		     

	  default: error(Vermisc,"Bad argument to float",nil,FALSE,0,handy);
	}
}

/* Lbreak ***************************************************************/
/* If first argument is not nil, this is evaluated and printed.  Then	*/
/* error is called with the "breaking" message.				*/
lispval Lbreak() {
	register lispval hold;

	if (np > lbot) {
		printr(lbot->val,poport);
		dmpport(poport);
	}
	return(error("",TRUE));
}


lispval LDivide() {
	register lispval result, work, temp;
	register struct argent *mynp;
	register struct argent *lbot, *np;
	int typ;
	lispval quo, rem; struct sdot dummy;

	chkarg(2);
	mynp = lbot;
	result = mynp->val;
	work = (mynp+1)->val;

	if((typ=TYPE(result))==INT) {
		protect(temp=newsdot());
		temp->i	= result->i;
		result = temp;
	} else if (typ!=SDOT)
		error("First arg to divide neither a bignum nor int.",FALSE);
	typ = TYPE(work);
	if(typ != INT && typ != SDOT)
		error("second arg to Divide neither an sdot nor an int.",FALSE);
	if(typ == INT) {
		dummy.CDR = (lispval) 0;
		dummy.I = work->i;
		work = (lispval) &dummy;
	}
	divbig(result,work, &quo, &rem);
	protect(quo);
	if(rem==((lispval) &dummy))
		protect(rem = inewint(dummy.I));
	protect(result = work = newdot());
	work->car = quo;
	(work->cdr = newdot())->car = rem;
	return(result);
}
lispval LEmuldiv(){
	register struct argent * mynp = lbot+AD;
	register lispval work, result;
	int quo, rem;
	snpand(3); /* fix register mask */

	/* (Emuldiv mul1 mult2 add quo) => 
		temp = mul1 + mul2 + sext(add);
		result = (list temp/quo temp%quo);
		to mix C and lisp a bit */

	Imuldiv(mynp[0].val->i, mynp[1].val->i, mynp[2].val->i,
		mynp[3].val->i, &quo, &rem);
	protect(result=newdot());
	(result->car=inewint(quo));
	work = result->cdr = newdot();
	(work->car=inewint(rem));
	return(result);
}
static Imuldiv() {
asm("	emul	4(ap),8(ap),12(ap),r0");
asm("	ediv	16(ap),r0,*20(ap),*24(ap)");
}


