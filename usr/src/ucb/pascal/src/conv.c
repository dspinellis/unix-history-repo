/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)conv.c	5.1 (Berkeley) 6/5/85";
#endif not lint

#include "whoami.h"
#ifdef PI
#include "0.h"
#include "opcode.h"
#ifdef PC
#   include	<pcc.h>
#endif PC
#include "tree_ty.h"

#ifndef PC
#ifndef PI0
/*
 * Convert a p1 into a p2.
 * Mostly used for different
 * length integers and "to real" conversions.
 */
convert(p1, p2)
	struct nl *p1, *p2;
{
	if (p1 == NLNIL || p2 == NLNIL)
		return;
	switch (width(p1) - width(p2)) {
		case -7:
		case -6:
			(void) put(1, O_STOD);
			return;
		case -4:
			(void) put(1, O_ITOD);
			return;
		case -3:
		case -2:
			(void) put(1, O_STOI);
			return;
		case -1:
		case 0:
		case 1:
			return;
		case 2:
		case 3:
			(void) put(1, O_ITOS);
			return;
		default:
			panic("convert");
	}
}
#endif 
#endif PC

/*
 * Compat tells whether
 * p1 and p2 are compatible
 * types for an assignment like
 * context, i.e. value parameters,
 * indicies for 'in', etc.
 */
compat(p1, p2, t)
	struct nl *p1, *p2;
	struct tnode *t;
{
	register c1, c2;

	c1 = classify(p1);
	if (c1 == NIL)
		return (NIL);
	c2 = classify(p2);
	if (c2 == NIL)
		return (NIL);
	switch (c1) {
		case TBOOL:
		case TCHAR:
			if (c1 == c2)
				return (1);
			break;
		case TINT:
			if (c2 == TINT)
				return (1);
		case TDOUBLE:
			if (c2 == TDOUBLE)
				return (1);
#ifndef PI0
			if (c2 == TINT && divflg == FALSE && t != TR_NIL ) {
				divchk= TRUE;
				c1 = classify(rvalue(t, NLNIL , RREQ ));
				divchk = FALSE;
				if (c1 == TINT) {
					error("Type clash: real is incompatible with integer");
					cerror("This resulted because you used '/' which always returns real rather");
					cerror("than 'div' which divides integers and returns integers");
					divflg = TRUE;
					return (NIL);
				}
			}
#endif
			break;
		case TSCAL:
			if (c2 != TSCAL)
				break;
			if (scalar(p1) != scalar(p2)) {
				derror("Type clash: non-identical scalar types");
				return (NIL);
			}
			return (1);
		case TSTR:
			if (c2 != TSTR)
				break;
			if (width(p1) != width(p2)) {
				derror("Type clash: unequal length strings");
				return (NIL);
			}
			return (1);
		case TNIL:
			if (c2 != TPTR)
				break;
			return (1);
		case TFILE:
			if (c1 != c2)
				break;
			derror("Type clash: files not allowed in this context");
			return (NIL);
		default:
			if (c1 != c2)
				break;
			if (p1 != p2) {
				derror("Type clash: non-identical %s types", clnames[c1]);
				return (NIL);
			}
			if (p1->nl_flags & NFILES) {
				derror("Type clash: %ss with file components not allowed in this context", clnames[c1]);
				return (NIL);
			}
			return (1);
	}
	derror("Type clash: %s is incompatible with %s", clnames[c1], clnames[c2]);
	return (NIL);
}

#ifndef PI0
#ifndef PC
/*
 * Rangechk generates code to
 * check if the type p on top
 * of the stack is in range for
 * assignment to a variable
 * of type q.
 */
rangechk(p, q)
	struct nl *p, *q;
{
	register struct nl *rp;
#ifdef OBJ
	register op;
	int wq, wrp;
#endif

	if (opt('t') == 0)
		return;
	rp = p;
	if (rp == NIL)
		return;
	if (q == NIL)
		return;
#	ifdef OBJ
	    /*
	     * When op is 1 we are checking length
	     * 4 numbers against length 2 bounds,
	     * and adding it to the opcode forces
	     * generation of appropriate tests.
	     */
	    op = 0;
	    wq = width(q);
	    wrp = width(rp);
	    op = wq != wrp && (wq == 4 || wrp == 4);
	    if (rp->class == TYPE || rp->class == CRANGE)
		    rp = rp->type;
	    switch (rp->class) {
	    case RANGE:
		    if (rp->range[0] != 0) {
#    		    ifndef DEBUG
			    if (wrp <= 2)
				    (void) put(3, O_RANG2+op, ( short ) rp->range[0],
						     ( short ) rp->range[1]);
			    else if (rp != nl+T4INT)
				    (void) put(3, O_RANG4+op, rp->range[0], rp->range[1] );
#    		    else
			    if (!hp21mx) {
				    if (wrp <= 2)
					    (void) put(3, O_RANG2+op,( short ) rp->range[0],
							    ( short ) rp->range[1]);
				    else if (rp != nl+T4INT)
					    (void) put(3, O_RANG4+op,rp->range[0],
							     rp->range[1]);
			    } else
				    if (rp != nl+T2INT && rp != nl+T4INT)
					    (void) put(3, O_RANG2+op,( short ) rp->range[0],
							    ( short ) rp->range[1]);
#    		    endif
			break;
		    }
		    /*
		     * Range whose lower bounds are
		     * zero can be treated as scalars.
		     */
	    case SCAL:
		    if (wrp <= 2)
			    (void) put(2, O_RSNG2+op, ( short ) rp->range[1]);
		    else
			    (void) put( 2 , O_RSNG4+op, rp->range[1]);
		    break;
	    default:
		    panic("rangechk");
	    }
#	endif OBJ
#	ifdef PC
		/*
		 *	pc uses precheck() and postcheck().
		 */
	    panic("rangechk()");
#	endif PC
}
#endif
#endif
#endif

#ifdef PC
    /*
     *	if type p requires a range check,
     *	    then put out the name of the checking function
     *	for the beginning of a function call which is completed by postcheck.
     *  (name1 is for a full check; name2 assumes a lower bound of zero)
     */
precheck( p , name1 , name2 )
    struct nl	*p;
    char	*name1 , *name2;
    {

	if ( opt( 't' ) == 0 ) {
	    return;
	}
	if ( p == NIL ) {
	    return;
	}
	if ( p -> class == TYPE ) {
	    p = p -> type;
	}
	switch ( p -> class ) {
	    case CRANGE:
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			    , name1);
		break;
	    case RANGE:
		if ( p != nl + T4INT ) {
		    putleaf( PCC_ICON , 0 , 0 ,
			    PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR ),
			    p -> range[0] != 0 ? name1 : name2 );
		}
		break;
	    case SCAL:
		    /*
		     *	how could a scalar ever be out of range?
		     */
		break;
	    default:
		panic( "precheck" );
		break;
	}
    }

    /*
     *	if type p requires a range check,
     *	    then put out the rest of the arguments of to the checking function
     *	a call to which was started by precheck.
     *	the first argument is what is being rangechecked (put out by rvalue),
     *	the second argument is the lower bound of the range,
     *	the third argument is the upper bound of the range.
     */
postcheck(need, have)
    struct nl	*need;
    struct nl	*have;
{
    struct nl	*p;

    if ( opt( 't' ) == 0 ) {
	return;
    }
    if ( need == NIL ) {
	return;
    }
    if ( need -> class == TYPE ) {
	need = need -> type;
    }
    switch ( need -> class ) {
	case RANGE:
	    if ( need != nl + T4INT ) {
		sconv(p2type(have), PCCT_INT);
		if (need -> range[0] != 0 ) {
		    putleaf( PCC_ICON , (int) need -> range[0] , 0 , PCCT_INT ,
							(char *) 0 );
		    putop( PCC_CM , PCCT_INT );
		}
		putleaf( PCC_ICON , (int) need -> range[1] , 0 , PCCT_INT ,
				(char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		sconv(PCCT_INT, p2type(have));
	    }
	    break;
	case CRANGE:
	    sconv(p2type(have), PCCT_INT);
	    p = need->nptr[0];
	    putRV(p->symbol, (p->nl_block & 037), p->value[0],
		    p->extra_flags, p2type( p ) );
	    putop( PCC_CM , PCCT_INT );
	    p = need->nptr[1];
	    putRV(p->symbol, (p->nl_block & 037), p->value[0],
		    p->extra_flags, p2type( p ) );
	    putop( PCC_CM , PCCT_INT );
	    putop( PCC_CALL , PCCT_INT );
	    sconv(PCCT_INT, p2type(have));
	    break;
	case SCAL:
	    break;
	default:
	    panic( "postcheck" );
	    break;
    }
}
#endif PC

#ifdef DEBUG
conv(dub)
	int *dub;
{
	int newfp[2];
	double *dp = ((double *) dub);
	long *lp = ((long *) dub);
	register int exp;
	long mant;

	newfp[0] = dub[0] & 0100000;
	newfp[1] = 0;
	if (*dp == 0.0)
		goto ret;
	exp = ((dub[0] >> 7) & 0377) - 0200;
	if (exp < 0) {
		newfp[1] = 1;
		exp = -exp;
	}
	if (exp > 63)
		exp = 63;
	dub[0] &= ~0177600;
	dub[0] |= 0200;
	mant = *lp;
	mant <<= 8;
	if (newfp[0])
		mant = -mant;
	newfp[0] |= (mant >> 17) & 077777;
	newfp[1] |= (((int) (mant >> 1)) & 0177400) | (exp << 1);
ret:
	dub[0] = newfp[0];
	dub[1] = newfp[1];
}
#endif
