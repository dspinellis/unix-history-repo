static char *sccsid = "@(#)lam4.c	35.2 7/8/81";

#include "global.h"
typedef struct argent *ap;
lispval adbig(),subbig(),mulbig();
double Ifloat();
lispval
Ladd()
{
	register lispval work;
	register ap result, mynp, oldnp, lbot, np;
	int restype,prunep,hi,lo=0;
	struct sdot dummybig;
	double flacc;

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
		    dmlad(result->val,1,work->i);
		    prunep = TRUE;
		    /* In adding the fixnum to the sdot we may make it
		    possible for the bignum to be represented as a fixnum */
		    break;
		case INT:
		    if(exarith(lo,1,work->i,&hi,&lo)) {
			work = result->val = newsdot();
			work->s.I = lo;
			work = work->s.CDR = newdot();
			work->s.I = hi;
			work->s.CDR = nil;
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
		    dummybig.CDR = nil;
		    work=adbig(work,&dummybig);
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
			handy = adbig(&dummybig,result->val);
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
		    errorh(Vermisc,"Non-number to add",nil,0,FALSE,work);
	    }
	}
	work = result->val;
	switch(restype){
	case DOUB:
		return(work);
	case INT:
		return(inewint(lo));
	case SDOT:
		if(prunep==FALSE)
		    return(work);
		else {
		    /* wouldn't (copy result->val) be faster ? -dhl */
		    /* It might, but isn't guaranteed to canonicalize */

		    dummybig.I = 0;
		    dummybig.CDR = (lispval) 0;
		    return(adbig(&dummybig,work));
		}
	urk:
		error("Internal error in add ",FALSE);
	}
}

/* exarith 	exact arithmetic
 * calculates x=a*b+c  where a,b and c are 32 bit 2's complement integers
 * whose top two bits must be the same (i.e. the are members of the set
 * of valid fixnum values for Franz Lisp).  The result, x, will be 64 bits
 * long but since each of a, b and c had only 31 bits of precision, the
 * result x only has 62 bits of precision.  The lower 30 bits are returned
 * in *plo and the high 32 bits are returned in *phi.  If *phi is 0 or -1 then
 * x doesn't need any more than 31 bits plus sign to describe, so we
 * place the sign in the high two bits of *plo and return 0 from this
 * routine.  A non zero return indicates that x requires more than 31 bits
 * to describe.
 */
exarith(a,b,c,phi,plo)
int *phi, *plo;
{
asm("	emul	4(ap),8(ap),12(ap),r2	#r2 = a*b + c to 64 bits");
asm("	extzv	$0,$30,r2,*20(ap)	#get new lo");
asm("	extv	$30,$32,r2,r0		#get new carry");
asm("	beql	out			# hi = 0, no work necessary");
asm("	movl	r0,*16(ap)		# save hi");
asm("	mcoml	r0,r0			# Is hi = -1 (it'll fit in one word)");
asm("	bneq	out			# it doesn't");
asm("	bisl2	$0xc0000000,*20(ap)	# alter low so that it is ok.");
asm("out:	ret");
}



lispval
Lsub()
{
	register lispval work;
	register ap result, mynp, oldnp, lbot, np;
	int prunep,restype,hi,lo=0;
	struct sdot dummybig;
	double flacc;
	lispval Lminus();

	oldnp = result = np;
	mynp = lbot + 1;
	restype = INT;
	prunep = TRUE;
	if(oldnp==lbot)
		goto out;
	if(oldnp==mynp) {
		return(Lminus());
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
		dummybig.CDR = nil;
		work = adbig(work,&dummybig);
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
				dmlad(result->val,1, -work->i);
				prunep = TRUE;  /* check up on exiting
				break;		/* that it didn't collapse */
			case INT:
				/* subtracting a fixnum from a fixnum,
				 * the result could turn into a bignum
				 */
		    		if(exarith(lo,1,-work->i,&hi,&lo)) {
				    work = result->val = newsdot();
				    work->s.I = lo;
				    work = work->s.CDR = newdot();
				    work->s.I = hi;
				    work->s.CDR = nil;
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
				handy = adbig(&dummybig,result->val);
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
			errorh(Vermisc,"Non-number to minus",nil,FALSE,0,work);
		}
	}
out:
	work = result->val;
	switch(restype){
	case DOUB:
		return(work);
	case INT:
		return(inewint(lo));
	case SDOT:
		if(prunep==FALSE)
		    return(work);
		else {
		    dummybig.I = 0;
		    dummybig.CDR = (lispval) 0;
		    return(adbig(&dummybig,work));
		}
	urk:
		error("Internal error in difference",FALSE);
	}
}

lispval
Ltimes()
{
	register lispval work;
	register ap result, mynp, oldnp, lbot, np;
	int itemp,restype,prunep,hi,lo=1;
	struct sdot dummybig;
	double flacc;

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
		    dmlad(result->val,work->i,0);
		    prunep = TRUE;
		    /* In adding the fixnum to the sdot we may make it
		    possible for the bignum to be represented as a fixnum */
		    break;
		case INT:
		    if(exarith(lo,work->i,0,&hi,&lo)) {
			work = result->val = newsdot();
			work->s.I = lo;
			work = work->s.CDR = newdot();
			work->s.I = hi;
			work->s.CDR = nil;
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
		    dummybig.CDR = nil;
		    work=mulbig(work,&dummybig);
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
			handy = adbig(&dummybig,result->val);
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
		    errorh(Vermisc,"Non-number to add",nil,0,FALSE,work);
	    }
	}
	work = result->val;
	switch(restype){
	case DOUB:
		return(work);
	case INT:
		return(inewint(lo));
	case SDOT:
		if(prunep==FALSE)
		    return(work);
		else {
		    /* wouldn't (copy result->val) be faster ? -dhl */
		    /* It might, but isn't guaranteed to canonicalize */

		    dummybig.I = 0;
		    dummybig.CDR = (lispval) 0;
		    return(adbig(&dummybig,work));
		}
	urk:
		error("Internal error in times",FALSE);
	}
}

lispval
Lquo()
{
	register lispval work;
	register ap result, mynp, oldnp, lbot, np;
	int restype; lispval quotient; double flacc;
	struct sdot dummybig;

	oldnp = result = np;
	protect(nil);
	mynp = lbot + 1;
	restype = INT;
	dummybig.I = 1; dummybig.CDR = (lispval) 0;

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
		work = adbig(work,&dummybig);
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
		errorh(Vermisc,"Internal quotient error #1: ",nil,FALSE,0,
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
		    divbig(result->val,&dummybig,&quotient,0);
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
		    errorh(Vermisc,"Internal quotient error #2: ",nil,FALSE,0,
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
		    divbig((lispval)&dummybig,work,&quotient,0);
		    goto on1;

		case SDOT:
		    /* dividing one bignum by another. */
		    divbig(result->val,work,&quotient,0);
		    pruneb(result->val);
		    goto on1;
		case DOUB:
		    /* dividing a bignum into a flonum.
		     */
		    result->val->r /= Ifloat(work);
		    break;
		default:
		    errorh(Vermisc,"Internal quotient error #3: ",nil,FALSE,0,
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
		        errorh(Vermisc,"Internal quotient error #4: ",nil,
						 FALSE,0, result->val);
			goto urk;
		}
		    break;
	    default:
		    errorh(Vermisc,"Non-number to quotient ",nil,FALSE,0,work);
	    }
	}
out:
	work = result->val;
	switch(restype){
	case SDOT:
	case DOUB:
	    return(work);
	case INT:
	    return(inewint(dummybig.I));
	default:
	urk:
	    errorh(Vermisc,"Internal quotient error #5: ",nil,FALSE,0,
					 work);
	}
}
lispval Lfp()
{
	register temp = 0, x, y;
	register struct argent *argp; 

	for(argp = lbot; argp < np; argp++)
		temp += argp->val->i;
	return(inewint(temp));
}

lispval Lfm()
{
	register temp, x, y;
	register struct argent *argp;

	if(lbot==np)return(inewint(0));
	temp = lbot->val->i;
	if(lbot+1==np) return(inewint(-temp));
	for(argp = lbot+1; argp < np; argp++)
		temp -= argp->val->i;
	return(inewint(temp));
}

lispval Lft()
{
	register temp = 1, x, y;
	register struct argent *argp;

	for(argp = lbot; argp < np; argp++)
		temp *= argp->val->i;
	return(inewint(temp));
}

lispval Lfd()
{
	register temp = 0, x, y;
	register struct argent *argp;

	if(lbot==np)return(inewint(1));
	temp = lbot->val->i;
	if(lbot+1==np) return(inewint(1/temp));
	for(argp = lbot+1; argp < np; argp++)
		temp /= argp->val->i;
	return(inewint(temp));
}
