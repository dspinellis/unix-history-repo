/* Copyright (c) 1980 Regents of the University of California */

static char sccsid[] = "@(#)flvalue.c 1.8 %G%";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
#   include "pc.h"
#   include "pcops.h"
#endif PC
#ifdef OBJ
/*
 * runtime display structure
 */
struct dispsave {
	char *locvars;		/* pointer to local variables */
	struct stack *stp;	/* pointer to local stack frame */
};
#endif OBJ

    /*
     *	flvalue generates the code to either pass on a formal routine,
     *	or construct the structure which is the environment for passing.
     *	it tells the difference by looking at the tree it's given.
     */
struct nl *
flvalue( r , formalp )
    int		*r;
    struct nl	*formalp;
    {
	struct nl	*p;
	long		tempoff;
	char		*typename;
#ifdef PC
	char		extname[ BUFSIZ ];
#endif PC

	if ( r == NIL ) {
	    return NIL;
	}
	typename = formalp -> class == FFUNC ? "function":"procedure";
	if ( r[0] != T_VAR ) {
	    error("Expression given, %s required for %s parameter %s" ,
		    typename , typename , formalp -> symbol );
	    return NIL;
	}
	p = lookup(r[2]);
	if (p == NIL) {
	    return NIL;
	}
	switch ( p -> class ) {
	    case FFUNC:
	    case FPROC:
		    if ( r[3] != NIL ) {
			error("Formal %s %s cannot be qualified" ,
				typename , p -> symbol );
			return NIL;
		    }
#		    ifdef OBJ
			put(2, PTR_RV | bn << 8+INDX, (int)p->value[NL_OFFS]);
#		    endif OBJ
#		    ifdef PC
			putRV( p -> symbol , bn , p -> value[ NL_OFFS ] , 
				p2type( p ) );
#		    endif PC
		    return p;
	    case FUNC:
	    case PROC:
		    if ( r[3] != NIL ) {
			error("%s %s cannot be qualified" , typename ,
				p -> symbol );
			return NIL;
		    }
		    if (bn == 0) {
			error("Built-in %s %s cannot be passed as a parameter" ,
				typename , p -> symbol );
			return NIL;
		    }
			/*
			 *	formal routine structure:
			 *
			 *	struct formalrtn {
			 *		long		(*entryaddr)();
			 *		long		cbn;
			 *		struct dispsave	disp[2*MAXLVL];
			 *	};
			 */
		    tempoff = tmpalloc(sizeof (long (*)()) + sizeof (long)
			+ 2*bn*sizeof (struct dispsave), nl+TSTR, NOREG);
#		    ifdef OBJ
			put(2 , O_LV | cbn << 8 + INDX , (int)tempoff );
			put(2, O_FSAV | bn << 8, (long)p->entloc);
#		    endif OBJ
#		    ifdef PC
			putleaf( P2ICON , 0 , 0 ,
			    ADDTYPE( P2PTR , ADDTYPE( P2FTN , P2PTR|P2STRTY ) ) ,
			    "_FSAV" );
			sextname( extname , p -> symbol , bn );
			putleaf( P2ICON , 0 , 0 , p2type( p ) , extname );
			putleaf( P2ICON , bn , 0 , P2INT , 0 );
			putop( P2LISTOP , P2INT );
			putLV( 0 , cbn , tempoff , P2STRTY );
			putop( P2LISTOP , P2INT );
			putop( P2CALL , P2PTR | P2STRTY );
#		    endif PC
		    return p;
	    default:
		    error("Variable given, %s required for %s parameter %s" ,
			    typename , typename , formalp -> symbol );
		    return NIL;
	}
    }
