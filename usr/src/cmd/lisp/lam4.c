static char *sccsid = "@(#)lam4.c	34.1 10/3/80";

#include "global.h"
typedef struct argent *ap;
static int prunep; lispval adbig(),subbig(),mulbig();
lispval
Ladd()
{
	register lispval work;
	register ap result, mynp, oldnp, lbot, np;
	int itemp,restype;

	oldnp = result = np;
	protect(rdrsdot);
	rdrsdot->s.CDR = (lispval) 0;
	rdrsdot->s.I =0;
	restype = SDOT;
	prunep = TRUE;

	for(mynp = lbot; mynp < oldnp; mynp++)
	{
		work = mynp->val;
		switch(TYPE(work)) {
		case INT:
			switch(restype) {
			case DOUB:
				result->val->r += work->i;
				break;
			case SDOT:
				dmlad(result->val,1,work->i);
				prunep = TRUE;
				break;
			default:
				goto urk;
			}
			break;
		case SDOT:
			switch(restype) {
			case DOUB:
				error("Don't know how to make bignums into reals, yet",FALSE);
				break;
			case SDOT:
				result->val = adbig(work,result->val);
				restype = TYPE(result->val);
				if(restype==INT) {
					rdrsdot->s.I=result->val->s.I;
					rdrsdot->s.CDR = (lispval) 0;
					result->val = rdrsdot;
					restype=SDOT;
					prunep = TRUE;
				} else
					prunep = FALSE;
				break;
			default:
				goto urk;
			}
			break;
		case DOUB:
			switch(restype) {
			case SDOT:
				if(result->val->s.CDR==(lispval) 0) {
					protect(newdoub());
					np[-1].val->r = result->val->i+work->r;
					result->val = np[-1].val;
					np--;
					restype = DOUB;
				} else 
					error("Don't know how to make bignums into reals, yet",FALSE);
				break;
			case DOUB:
				result->val->r += work->r;
				break;
			default:
				goto urk;
			}
			break;
		default:
			errorh(Vermisc,"Non-number to add",nil,0,FALSE,work);
		}
	}
	if(restype==DOUB || prunep==FALSE)
		return(result->val);
	else if (result->val->s.CDR==(lispval) 0)
		return(inewint(result->val->s.I));
	else {
		struct sdot dummybig;

		dummybig.I = 0;
		dummybig.CDR = (lispval) 0;
		return(adbig(&dummybig,result->val));
	}
	urk:
		error("Internal error in (add,sub,quo,times)",FALSE);
}
lispval
Lsub()
{
	register lispval work;
	register ap result, mynp, oldnp, lbot, np;
	int itemp,restype;
	lispval Lminus();

	oldnp = result = np;
	mynp = lbot + 1;
	protect(rdrsdot);
	rdrsdot->s.CDR = (lispval) 0;
	rdrsdot->s.I =0;
	restype = SDOT;
	prunep = TRUE;
	if(oldnp==lbot)
		goto out;
	if(oldnp==mynp) {
		np--;
		return(Lminus());
	}
	work = lbot->val;
	switch(TYPE(work)) {
	case INT:
		rdrsdot->s.I = work->i;
		break;
	case SDOT:
		result->val = adbig(result->val,work);
		if(TYPE(result->val)==INT) {
			rdrsdot->s.I = result->val->i;
			result->val = rdrsdot;
		}
		break;
	case DOUB:
		(result->val = newdoub())->r = work->r;
		restype = DOUB;
	}

	for(; mynp < oldnp; mynp++)
	{
		work = mynp->val;
		switch(TYPE(work)) {
		case INT:
			switch(restype) {
			case DOUB:
				result->val->r -= work->i;
				break;
			case SDOT:
				dmlad(result->val,1, -work->i);
				prunep = TRUE;
				break;
			default:
				goto urk;
			}
			break;
		case SDOT:
			switch(restype) {
			case DOUB:
				errorh(Vermisc,
				       "difference: Don't know how to make bignums into reals, yet",
				       nil,FALSE,0,work);
				break;
			case SDOT:
				result->val = subbig(result->val,work);
				restype = TYPE(result->val);
				if(restype==INT) {
					rdrsdot->s.I=result->val->s.I;
					rdrsdot->s.CDR = (lispval) 0;
					result->val = rdrsdot;
					restype=SDOT;
					prunep = TRUE;
				} else
					prunep = FALSE;
				break;
			default:
				goto urk;
			}
			break;
		case DOUB:
			switch(restype) {
			case SDOT:
				if(result->val->s.CDR==(lispval) 0) {
					protect(newdoub());
					np[-1].val->r = result->val->i-work->r;
					result->val = np[-1].val;
					np--;
					restype = DOUB;
				} else 
					errorh(Vermisc,
					       "difference: Don't know how to make bignums into reals ",nil,FALSE,0,work);
				break;
			case DOUB:
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
	if(restype==DOUB || prunep==FALSE)
		return(result->val);
	else if (result->val->s.CDR==(lispval) 0)
		return(inewint(result->val->s.I));
	else {
		struct sdot dummybig;

		dummybig.I = 0;
		dummybig.CDR = (lispval) 0;
		return(adbig(&dummybig,result->val));
	}
	urk:
		error("Internal error in (add,sub,quo,times)",FALSE);
}
lispval
Ltimes()
{
	register lispval work;
	register ap result, mynp, oldnp, lbot, np;
	int itemp,restype;

	oldnp = result = np;
	protect(rdrsdot);
	rdrsdot->s.CDR = (lispval) 0;
	rdrsdot->s.I = 1;
	restype = SDOT;
	prunep = TRUE;

	for(mynp = lbot; mynp < oldnp; mynp++)
	{
		work = mynp->val;
		switch(TYPE(work)) {
		case INT:
			switch(restype) {
			case DOUB:
				result->val->r *= work->i;
				break;
			case SDOT:
				dmlad(result->val,work->i,0);
				prunep = TRUE;
				break;
			default:
				goto urk;
			}
			break;
		case SDOT:
			switch(restype) {
			case DOUB:
				error("Don't know how to make bignums into reals, yet",FALSE);
				break;
			case SDOT:
				result->val = mulbig(work,result->val);
				restype = TYPE(result->val);
				if(restype==INT) {
					if(result->val->i==0)
						return(result->val);
					rdrsdot->s.I=result->val->s.I;
					rdrsdot->s.CDR = (lispval) 0;
					result->val = rdrsdot;
					restype=SDOT;
					prunep = TRUE;
				} else
					prunep = FALSE;
				break;
			default:
				goto urk;
			}
			break;
		case DOUB:
			switch(restype) {
			case SDOT:
				if(result->val->s.CDR==(lispval) 0) {
					protect(newdoub());
					np[-1].val->r = result->val->i*work->r;
					result->val = np[-1].val;
					np--;
					restype = DOUB;
				} else 
					error("Don't know how to make bignums into reals, yet",FALSE);
				break;
			case DOUB:
				result->val->r *= work->r;
				break;
			default:
				goto urk;
			}
			break;
		default:
			error("Non-number to times",FALSE);
		}
	}
	if(restype==DOUB || prunep==FALSE)
		return(result->val);
	else if (result->val->s.CDR==(lispval) 0)
		return(inewint(result->val->s.I));
	else {
		struct sdot dummybig;

		dummybig.I = 0;
		dummybig.CDR = (lispval) 0;
		return(adbig(&dummybig,result->val));
	}
	urk:
		error("Internal error in (add,sub,quo,times)",FALSE);
}
lispval
Lquo()
{
	register lispval work;
	register lispval result;
	register struct argent *mynp;
	register struct argent *oldnp, *lbot, *np;
	int bigflag = 0, realflag = 0, itemp;
	struct sdot dummybig;
	lispval divbig(), *resaddr;

	mynp = lbot;
	oldnp = np-1;
	dummybig.CDR = (lispval) 0;
	dummybig.I = 1;
	if(mynp > oldnp) goto out;
	work = (mynp++)->val;
	itemp = TYPE(work);
	switch(itemp) {
	case INT:
		if(mynp <= oldnp) dummybig.I = work->i;
		else dummybig.I = 1/work->i;
		break;
	case DOUB:
		realflag = 1;
		protect(result = newdoub());
		if(mynp <= oldnp) result->r = work->r;
		else result->r = 1.0/work->r;
		break;
	case SDOT: /* must be fixed for the inverse case */
		protect(work);
		resaddr = &(np[-1].val);
		bigflag = 1;
		break;
	default:
		error("Don't know how to divide this type.",FALSE);
	}
	for(;mynp <= oldnp; mynp++) {
		work = mynp->val;
		itemp = TYPE(work);
		switch(itemp) {

		case INT:
			if (work->i==0)
				kill(getpid(),8);
			if (realflag)
				result->r /= work->i;
			else if(bigflag) {
				dummybig.I = work->i;
				divbig(*resaddr, &dummybig, resaddr, 0);
			} else {
				dummybig.I /= work->i;
			}
			break;
		case DOUB:
			if(realflag)
				result->r /= work->r;
			else if(bigflag)
				error("Don't know how to make bignums into reals, yet",FALSE);
			else {
				realflag = 1;
				result = newdoub();
				result->r = (double) dummybig.I / work->r;
				protect(result);
			}
			break;
		case SDOT:
			if(realflag)
				error("Don't know how to divide reals by bignums ",FALSE);
			else if(bigflag)
				divbig(*resaddr, work, resaddr, 0);
			else {
				bigflag = 1;
				protect(newsdot());
				resaddr = &(np[-1].val);
				np[-1].val->i = dummybig.I;
				divbig(*resaddr, work, resaddr, 0);
			}
			break;
		default:
			error("Don't know how to divide this type",FALSE);

		}
	}
out:
	if(realflag)
		return(result);
	else if (bigflag)
		return(*resaddr);
	else {
		result = inewint(  dummybig.I );
		return(result);
	}
}
