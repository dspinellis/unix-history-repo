#ifndef lint
static char *rcsid =
   "$Header: lam4.c,v 1.5 83/12/28 16:21:08 sklower Exp $";
#endif

/*					-[Sun Jun 19 22:25:48 1983 by jkf]-
 * 	lam4.c				$Locker:  $
 * lambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include "global.h"
lispval adbig(),subbig(),mulbig();
double Ifloat();
lispval
Ladd()
{
	register lispval work;
	register struct argent *result, *mynp, *oldnp;
	long restype,prunep,hi,lo=0;
	struct sdot dummybig;
	double flacc;
	Savestack(4);

	oldnp = result = np;
	restype = INT;			/* now start as integers */
	protect(nil);

	for(mynp = lbot; mynp < oldnp; mynp++)
	{
	    work = mynp->val;
	    switch(TYPE(work)) {
	    case INT:
		switch(restype) {
		case SDOT:
		    dmlad(result->val,1L,work->i);
		    prunep = TRUE;
		    /* In adding the fixnum to the sdot we may make it
		    possible for the bignum to be represented as a fixnum */
		    break;
		case INT:
		    if(exarith(lo,1L,work->i,&hi,&lo)) {
			work = result->val = newsdot();
			work->s.I = lo;
			work = work->s.CDR = newdot();
			work->s.I = hi;
			work->s.CDR = 0;
			restype = SDOT; prunep = FALSE;
		    } 
		    break;
		case DOUB:
		    result->val->r += work->i;
		    break;
		default: goto urk;
		}
		break;
	    case SDOT:
		switch(restype) {
		case INT:
		    dummybig.I = lo;
		    dummybig.CDR = 0;
		    work=adbig(work,(lispval)&dummybig);
		    goto code1;
		case SDOT:
		    work=adbig(work,result->val);
		    /* previous result is no longer needed */
		    pruneb(result->val);
		code1:
		    restype = TYPE(work); /* SDOT or INT */
		    if(restype==INT) {
			lo = work->i;
			prunei(work);
		    } else {
			prunep = FALSE; /* sdot is cannonical */
			result->val = work;
		    } break;
		case DOUB:
		    result->val->r += Ifloat(work);
		    break;
		default: goto urk;
		}
		break;
	    case DOUB:
		switch(restype) {
		case SDOT:
		    if(prunep) {
			lispval handy;
			dummybig.I = 0;
			dummybig.CDR = (lispval) 0;
			handy = adbig((lispval)&dummybig,result->val);
			pruneb(result->val);
			result->val = handy;
		    }
		    flacc = Ifloat(result->val) + work->r;
		    pruneb(result->val);
		scrimp:
		    (result->val = newdoub())->r = flacc;
		    restype = DOUB;
		    break;
		case INT:
		    flacc = work->r + lo;
		    goto scrimp;
		case DOUB:
		    result->val->r += work->r;
		    break;
		default: goto urk;
		}
		break;
	    default:
		    errorh1(Vermisc,"Non-number to add",nil,0,FALSE,work);
	    }
	}
	work = result->val;
	switch(restype){
	case DOUB:
		break;
	case INT:
		work=inewint(lo);
		break;
	case SDOT:
		if(prunep) {
		    /* wouldn't (copy result->val) be faster ? -dhl */
		    /* It might, but isn't guaranteed to canonicalize */

		    dummybig.I = 0;
		    dummybig.CDR = (lispval) 0;
		    work = adbig((lispval)&dummybig,work);
		}
		break;
	default:
	urk:
		error("Internal error in add ",FALSE);
	}
	Restorestack();
	return(work);
}

/* exarith(a,b,c,lo,hi)
 * int a,b,c;
 * int *lo, *hi;
 * Exact arithmetic.
 * a,b and c are 32 bit 2's complement integers
 * calculates x=a*b+c to twice the precision of an int.
 * In the vax version, the 30 low bits only are returned
 * in *lo,and the next 32 bits of precision are returned in * hi.
 * this works since exarith is used either for calculating the sum of
 * two 32 bit numbers, (which is at most 33 bits), or
 * multiplying a 30 bit number by a 32 bit numbers,
 * which has a maximum precision of 62 bits.
 * If *phi is 0 or -1 then
 * x doesn't need any more than 31 bits plus sign to describe, so we
 * place the sign in the high two bits of *plo and return 0 from this
 * routine.  A non zero return indicates that x requires more than 31 bits
 * to describe.
 *
 * The definition has been moved to vax.c.
 */


lispval
Lsub()
{
	register lispval work;
	register struct argent *result, *mynp, *oldnp;
	long prunep,restype,hi,lo=0;
	struct sdot dummybig;
	double flacc;
	lispval Lminus();
	Savestack(4);

	oldnp = result = np;
	mynp = lbot + 1;
	restype = INT;
	prunep = TRUE;
	if(oldnp==lbot)
		goto out;
	if(oldnp==mynp) {
		work = Lminus();
		Restorestack();
		return(work);
	}
	protect(nil);
	work = lbot->val;

	/* examine the first argument and perhaps set restype to the 
	 * correct type.  If restype (result type) is INT, then the
	 * fixnum value is stored in lo.  Otherwise, if restype is 
	 * SDOT or DOUB, then the value is stored in result->val.
	 */
	switch(TYPE(work)) {
	case INT:
		lo = work->i;
		restype = INT;
		break;
	case SDOT:
		/* we want to copy the sdot we are given as an argument since
		 * the bignum arithmetic routine dmlad clobbers the values it
		 * is given.
		 */
		dummybig.I = 0;		/* create a zero sdot */
		dummybig.CDR = 0;
		work = adbig(work,(lispval)&dummybig);
		/* the resulting value may have been reduced from an
		 * sdot to a fixnum.  This should never happen though
		 * but if it does, we simplify things.
		 */
		restype = TYPE(work);
		if(restype==INT) {
		    lo = work->i;	/* has turned into an fixnum */
		    prunei(work);	/* return fixnum cell	     */
		} else {
		    prunep = FALSE; 	/* sdot is cannonical */
		    result->val = work;
		} 
		break;

	case DOUB:
		(result->val = newdoub())->r = work->r;
		restype = DOUB;
	}

	/* now loop through the rest of the arguments subtracting them
	 * from the running result in result or lo
	 */
	for(; mynp < oldnp; mynp++)
	{
		work = mynp->val;
		switch(TYPE(work)) {
		case INT:
			switch(restype) {
			case SDOT:
				/* subtracting a fixnum from an bignum
				 * use the distructive multiply (by 1)
				 * and add the negative of the work value.
				 * The result will still be pointed to
				 * by result->val
				 */
				dmlad(result->val,1L, -work->i);
				prunep = TRUE;  /* check up on exiting */
				break;		/* that it didn't collapse */
			case INT:
				/* subtracting a fixnum from a fixnum,
				 * the result could turn into a bignum
				 */
		    		if(exarith(lo,1L,-work->i,&hi,&lo)) {
				    work = result->val = newsdot();
				    work->s.I = lo;
				    work = work->s.CDR = newdot();
				    work->s.I = hi;
				    work->s.CDR = 0;
				    restype = SDOT; prunep = TRUE;
				}
				break;
			case DOUB:
				/* subtracting a fixnum from a flonum */
				result->val->r -= work->i;
				break;
			default:
				goto urk;
			}
			break;
		case SDOT:
			switch(restype) {
			case INT:
			    /* subtracting a bignum from an integer
			     * first make a bignum of the integer and
			     * then fall into the next case
			     */
			    dummybig.I = lo;
			    dummybig.CDR = (lispval) 0;
			    work = subbig((lispval)&dummybig,work);
			    goto on1;

			case SDOT:
			    /* subtracting one bignum from another.  The
			     * routine to do this ends up calling addbig
			     * and should probably be written specifically
			     * for subtraction.
			     */
			     work = subbig(result->val,work);
			     pruneb(result->val);
			on1:
			     /* check if the result has turned into a fixnum */
			     restype = TYPE(work);
			     if(restype==INT) {
				lo = work->i;		/* it has */
				prunei(work);
			     } else {
				prunep = FALSE; 	/* sdot is cannonical */
				result->val = work;
			     } 
			     break;
			case DOUB: /* Subtract bignum from float */
				   /* Death on overflow 	 */
			    result->val->r -= Ifloat(work);
			    break;
			default:
				goto urk;
			}
			break;

		case DOUB:
			switch(restype) {
			case SDOT:  /* subtracting a flonum from a bignum. */

			    if(prunep) {
				lispval handy;
				dummybig.I = 0;
				dummybig.CDR = (lispval) 0;
				handy = adbig((lispval)&dummybig,result->val);
				pruneb(result->val);
				result->val = handy;
			    }
			    flacc = Ifloat(result->val) - work->r;
			    pruneb(result->val);
		scrimp:	    (result->val = newdoub())->r = flacc;
			    restype = DOUB;
			    break;
			case INT:
				/* subtracting a flonum from an fixnum. 
				 * The result will be an flonum.
				 */
				flacc = lo - work->r;
				goto scrimp;
			case DOUB:
				/* subtracting a flonum from a flonum, what
				 * could be easier?
				 */
				result->val->r -= work->r;
				break;
			default:
				goto urk;
			}
			break;
		default:
			errorh1(Vermisc,"Non-number to minus",nil,FALSE,0,work);
		}
	}
out:
	work = result->val;
	switch(restype){
	case DOUB:
		break;
	case INT:
		work = inewint(lo);
		break;
	case SDOT:
		if(prunep) {
		    dummybig.I = 0;
		    dummybig.CDR = (lispval) 0;
		    work = adbig((lispval)&dummybig,work);
		}
		break;
	default:
	urk:
		error("Internal error in difference",FALSE);
	}
	Restorestack();
	return(work);
}

lispval
Ltimes()
{
	register lispval work;
	register struct argent *result, *mynp, *oldnp;
	long restype,prunep,hi,lo=1;
	struct sdot dummybig;
	double flacc;
	Savestack(4);

	oldnp = result = np;
	restype = INT;			/* now start as integers */
	prunep = TRUE;
	protect(nil);

	for(mynp = lbot; mynp < oldnp; mynp++)
	{
	    work = mynp->val;
	    switch(TYPE(work)) {
	    case INT:
		switch(restype) {
		case SDOT:
		    dmlad(result->val,work->i,0L);
		    prunep = TRUE;
		    /* In adding the fixnum to the sdot we may make it
		    possible for the bignum to be represented as a fixnum */
		    break;
		case INT:
		    if(exarith(lo,work->i,0L,&hi,&lo)) {
			work = result->val = newsdot();
			work->s.I = lo;
			work = work->s.CDR = newdot();
			work->s.I = hi;
			work->s.CDR = 0;
			restype = SDOT; prunep = TRUE;
		    } 
		    break;
		case DOUB:
		    result->val->r *= work->i;
		    break;
		default: goto urk;
		}
		break;
	    case SDOT:
		switch(restype) {
		case INT:
		    dummybig.I = lo;
		    dummybig.CDR = 0;
		    work=mulbig(work,(lispval)&dummybig);
		    goto code1;
		case SDOT:
		    work=mulbig(work,result->val);
		    /* previous result is no longer needed */
		    pruneb(result->val);
		code1:
		    restype = TYPE(work); /* SDOT or INT */
		    if(restype==INT) {
			lo = work->i;
			prunei(work);
		    } else {
			prunep = FALSE; /* sdot is cannonical */
			result->val = work;
		    } break;
		case DOUB:
		    result->val->r *= Ifloat(work);
		    break;
		default: goto urk;
		}
		break;
	    case DOUB:
		switch(restype) {
		case SDOT:
		    if(prunep) {
			lispval handy;
			dummybig.I = 0;
			dummybig.CDR = (lispval) 0;
			handy = adbig((lispval)&dummybig,result->val);
			pruneb(result->val);
			result->val = handy;
		    }
		    flacc = Ifloat(result->val) * work->r;
		    pruneb(result->val);
	scrimp:	    (result->val = newdoub())->r = flacc;
		    restype = DOUB;
		    break;
		case INT:
		    flacc = work->r * lo;
		    goto scrimp;
		case DOUB:
		    result->val->r *= work->r;
		    break;
		default: goto urk;
		}
		break;
	    default:
		    errorh1(Vermisc,"Non-number to add",nil,0,FALSE,work);
	    }
	}
	work = result->val;
	switch(restype){
	case DOUB:
		break;
	case INT:
		work = inewint(lo);
		break;
	case SDOT:
		if(prunep) {
		    dummybig.I = 0;
		    dummybig.CDR = (lispval) 0;
		    work = adbig((lispval)&dummybig,work);
		}
		break;
	default:
	urk:
		error("Internal error in times",FALSE);
	}
	Restorestack();
	return(work);
}

lispval
Lquo()
{
	register lispval work;
	register struct argent *result, *mynp, *oldnp;
	int restype; lispval quotient; double flacc;
	struct sdot dummybig;
	Savestack(4);

	oldnp = result = np;
	protect(nil);
	mynp = lbot + 1;
	restype = INT;
	dummybig.I = 1; dummybig.CDR = (lispval) 0;

	if(oldnp==lbot) goto out;
	if(oldnp==mynp) mynp = lbot;
	else {
	    /* examine the first argument and perhaps set restype to the 
	     * correct type.  If restype (result type) is INT, then the
	     * fixnum value is stored in lo.  Otherwise, if restype is 
	     * SDOT or DOUB, then the value is stored in result->val.
	     */
	    work = lbot->val;
	    switch(TYPE(work)) {
	    case INT:
		dummybig.I = work->i;
		break;
	    case SDOT:
		/* we want to copy the sdot we are given as an argument since
		 * the bignum divide routine divbig expects an argument in
		 * canonical form.
		 */
		dummybig.I = 0;		/* create a zero sdot */
		work = adbig(work,(lispval)&dummybig);
		restype = TYPE(work);
		if(restype==INT) {	/* Either INT or SDOT */
		    dummybig.I=work->i;	/* has turned into an fixnum */
		    prunei(work);	/* return fixnum cell	     */
		} else {
		    result->val = work;
		} 
		break;
	    case DOUB:
		(result->val = newdoub())->r = work->r;
		restype = DOUB;
		break;
	    default:
		errorh1(Vermisc,"Internal quotient error #1: ",nil,FALSE,0,
					 work);
		goto urk;
	    }
	}

	/* now loop through the rest of the arguments dividing them
	 * into the running result in result or dummybig.I
	 */
	for(; mynp < oldnp; mynp++)
	{
	    work = mynp->val;
	    switch(TYPE(work)) {
	    case INT:
		if (work->i==0)
		    kill(getpid(),8);
		switch(restype) {
		case SDOT:	/* there is no fast routine to destructively
				   divide a bignum by an int, so do it the
				   hard way. */
		    dummybig.I = work->i;
		    divbig(result->val,(lispval)&dummybig,&quotient,(lispval *)0);
		    pruneb(result->val);
		on1:
		    /* check if the result has turned into a fixnum */
		    restype = TYPE(quotient);
		    if(restype==INT) {		/* Either INT or SDOT */
			dummybig.I=quotient->i;	/* has turned into an fixnum */
			prunei(quotient);	/* return fixnum cell	     */
		    } else
			result->val = quotient;
		    break;
		case INT:	/* divide int by int */
		    dummybig.I /= work->i;
		    break;
		case DOUB:
		    result->val->r /= work->i;
		    break;
		default:
		    errorh1(Vermisc,"Internal quotient error #2: ",nil,FALSE,0,
					 result->val);
		    goto urk;
		}
		break;
	    case SDOT:
		switch(restype) {
		case INT:
		    /* Although it seems that dividing an int
		     * by a bignum can only lead to zero, it is
		     * concievable that the bignum is improperly boxed,
		     * i.e. actually an int.
		     */
		    divbig((lispval)&dummybig,work,&quotient,(lispval *)0);
		    goto on1;

		case SDOT:
		    /* dividing one bignum by another. */
		    divbig(result->val,work,&quotient,(lispval *)0);
		    pruneb(result->val);
		    goto on1;
		case DOUB:
		    /* dividing a bignum into a flonum.
		     */
		    result->val->r /= Ifloat(work);
		    break;
		default:
		    errorh1(Vermisc,"Internal quotient error #3: ",nil,FALSE,0,
					 result->val);
		    goto urk;
		}
		break;

	    case DOUB:
		switch(restype) {
		case SDOT: /* Divide bignum by flonum converting to flonum
			    * May die due to overflow */
		    flacc = Ifloat(result->val) / work->r;
		    pruneb(result->val);
		scrimp:
		    (result->val = newdoub())->r = flacc;
		    restype = DOUB;
		    break;
		case INT: /* dividing a flonum into a fixnum. 
			   * The result will be a flonum. */

		    flacc = ((double) dummybig.I) / work->r;
		    goto scrimp;
		case DOUB: /* dividing a flonum into a flonum, what
			    * could be easier?
			    */
		    result->val->r /= work->r;
		    break;
		default:
		        errorh1(Vermisc,"Internal quotient error #4: ",nil,
						 FALSE,0, result->val);
			goto urk;
		}
		    break;
	    default:
		    errorh1(Vermisc,"Non-number to quotient ",nil,FALSE,0,work);
	    }
	}
out:
	work = result->val;
	switch(restype){
	case SDOT:
	case DOUB:
	    break;
	case INT:
	    work = inewint(dummybig.I);
	    break;
	default:
	urk:
	    errorh1(Vermisc,"Internal quotient error #5: ",nil,FALSE,0,
					 work);
	}
	Restorestack();
	return(work);
}


lispval Lfp()
{
	register temp = 0;
	register struct argent *argp; 

	for(argp = lbot; argp < np; argp++)
	    if(TYPE(argp->val) != INT)
	    	errorh1(Vermisc,"+: non fixnum argument ",
				nil,FALSE,0,argp->val);
	    else
		temp += argp->val->i;
	return(inewint(temp));
}

lispval Lfm()
{
	register temp;
	register struct argent *argp;

	if(lbot==np)return(inewint(0));
	   if(TYPE(lbot->val) != INT)
	   	errorh1(Vermisc,"-: non fixnum argument ",
				nil,FALSE,0,lbot->val);
	   else
	       temp = lbot->val->i;
	if(lbot+1==np) return(inewint(-temp));
	for(argp = lbot+1; argp < np; argp++)
	    if(TYPE(argp->val) != INT)
	        errorh1(Vermisc,"-: non fixnum argument ",
				nil,FALSE,0,argp->val);
	    else
		temp -= argp->val->i;
	return(inewint(temp));
}

lispval Lft()
{
	register temp = 1;
	register struct argent *argp;

	for(argp = lbot; argp < np; argp++)
	    if(TYPE(argp->val) != INT)
	        errorh1(Vermisc,"*: non fixnum argument ",
				nil,FALSE,0,argp->val);
	    else
		temp *= argp->val->i;
	return(inewint(temp));
}

lispval Lflessp()
{
	register struct argent *argp = lbot;
	register old, new;

	if(np < argp + 2) return(nil);
	old = argp->val->i; argp++;
	for(; argp < np; argp++)
		if(TYPE(argp->val) != INT)
			errorh1(Vermisc,"<: non fixnum argument ",
			nil,FALSE,0,argp->val);
		else {
			new = argp->val->i;
			if(!(old < new)) return(nil);
			old = new;
		}
	return(tatom);
}

lispval Lfd()
{
	register temp = 0;
	register struct argent *argp;

	if(lbot==np)return(inewint(1));
	if(TYPE(lbot->val) != INT)
	    errorh1(Vermisc,"/: non fixnum argument ",
	    		nil,FALSE,0,lbot->val);
	temp = lbot->val->i;
	if(lbot+1==np) return(inewint(1/temp));
	for(argp = lbot+1; argp < np; argp++)
	    if(TYPE(argp->val) != INT)
	        errorh1(Vermisc,"/: non fixnum argument ",
	    		nil,FALSE,0,argp->val);
	    else
		temp /= argp->val->i;
	return(inewint(temp));
}

lispval Lfadd1()
{
    chkarg(1,"1+");
    if(TYPE(lbot->val) != INT)
        errorh1(Vermisc,"1+: non fixnum argument ",
			nil,FALSE,0,lbot->val);
    return(inewint(lbot->val->i + 1));
}

/*
 * Lfexpt	(^ 'x_a 'x_b)
 *   exponentiation of fixnums x_a and x_b returning a fixnum
 * result
 */
lispval Lfexpt()
{
    register int base;
    register int exp;
    register int res;
    
    chkarg(2,"^");
    if((TYPE(lbot[0].val) != INT ) || (TYPE(lbot[1].val) != INT))
      errorh2(Vermisc,"^: non fixnum arguments", nil,0,
      		lbot[0].val,lbot[1].val);
		
    base = lbot[0].val->i;
    exp = lbot[1].val->i;

    if(base == 0)
    {
	/* 0^0 == 1,  0 to any other power (even negative powers)
	 *  is zero (according to Maclisp)
	 */
	if(exp == 0) return(inewint(1));
	else return(inewint(0));
    }
    else if(base == 1)
        /*
	 *  1 to any power is 1
	 */
	return(lbot[0].val);	/* == 1 */
    else if(exp == 0)
    	/*
	 * anything to the zero power is 1
	 */
	return(inewint(1));
    else if(base == -1)
    {
        /*
	 * -1 to an even power is 1, to an odd is -1
	 */
	if(exp & 1) return(lbot[0].val);
	else return(inewint(1));
    }
    else if(exp < 0)
        /*
	 * anything not 0,-1,or 1  to a negative power is 0
	 *
	 */
	 return(inewint(0));

    /* compute exponentiation.  This should check for overflows,
       I suppose. --jkf
     */
    res = 1;
    while( exp > 0)
    {
	if( exp & 1 )
	{   /* odd, just multiply by one */
	    res = res * base;
	    exp--;
	}
	else {
	    /* even, square base */
	    base = base * base;
	    exp = exp / 2;
	}
    }
    return(inewint(res));
}
	    
    

lispval Lfsub1()
{
    chkarg(1,"1-");
    if(TYPE(lbot->val) != INT)
        errorh1(Vermisc,"1-: non fixnum argument ",
			nil,FALSE,0,lbot->val);
    return(inewint(lbot->val->i - 1));
}

lispval
Ldbtofl()
{
	float x;
	chkarg(1,"double-to-float");

	if(TYPE(lbot->val) != DOUB)
        errorh1(Vermisc,"double-to-float: non flonum argument ",
			nil,FALSE,0,lbot->val);
	x = lbot->val->r;
	return(inewint(*(long *)&x));
}

lispval
Lfltodb()
{
	register lispval handy;
	chkarg(1,"float-to-double");

	if(TYPE(lbot->val) != INT)
        errorh1(Vermisc,"float-to-double: non fixnum argument ",
			nil,FALSE,0,lbot->val);
	handy = newdoub();
	handy->r = *(float *)lbot->val;
	return(handy);
}
