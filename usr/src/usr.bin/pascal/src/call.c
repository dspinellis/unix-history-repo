/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)call.c 1.7 %G%";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
#   include "pc.h"
#   include "pcops.h"
#endif PC

short	slenline = 0;
short	floatline = 0;

/*
 * Call generates code for calls to
 * user defined procedures and functions
 * and is called by proc and funccod.
 * P is the result of the lookup
 * of the procedure/function symbol,
 * and porf is PROC or FUNC.
 * Psbn is the block number of p.
 *
 *	the idea here is that regular scalar functions are just called,
 *	while structure functions and formal functions have their results
 *	stored in a temporary after the call.
 *	structure functions do this because they return pointers
 *	to static results, so we copy the static
 *	and return a pointer to the copy.
 *	formal functions do this because we have to save the result
 *	around a call to the runtime routine which restores the display,
 *	so we can't just leave the result lying around in registers.
 *	so PROCs and scalar FUNCs look like
 *		p(...args...)
 *	structure FUNCs look like
 *		(temp = p(...args...),&temp)
 *	formal FPROCs look like
 *		((FCALL( p ))(...args...),FRTN( p ))
 *	formal scalar FFUNCs look like
 *		(temp = (FCALL( p ))(...args...),FRTN( p ),temp)
 *	formal structure FFUNCs look like
 *		(temp = (FCALL( p ))(...args...),FRTN( p ),&temp)
 */
struct nl *
call(p, argv, porf, psbn)
	struct nl *p;
	int *argv, porf, psbn;
{
	register struct nl *p1, *q;
	int *r;
	struct nl	*p_type_class = classify( p -> type );

#	ifdef OBJ
	    int		cnt;
#	endif OBJ
#	ifdef PC
	    long	p_p2type = p2type( p );
	    long	p_type_p2type = p2type( p -> type );
	    bool	noarguments;
	    long	calltype;	/* type of the call */
		/*
		 *	these get used if temporaries and structures are used
		 */
	    long	tempoffset;
	    long	temptype;	/* type of the temporary */
	    long	p_type_width;
	    long	p_type_align;
#	endif PC

#	ifdef OBJ
	    if (p->class == FFUNC || p->class == FPROC)
		put(2, PTR_RV | cbn << 8+INDX, (int)p->value[NL_OFFS]);
	    if (porf == FUNC)
		    /*
		     * Push some space
		     * for the function return type
		     */
		    put(2, O_PUSH, leven(-lwidth(p->type)));
#	endif OBJ
#	ifdef PC
		/*
		 *	if we have to store a temporary,
		 *	temptype will be its type,
		 *	otherwise, it's P2UNDEF.
		 */
	    temptype = P2UNDEF;
	    calltype = P2INT;
	    if ( porf == FUNC ) {
		p_type_width = width( p -> type );
		switch( p_type_class ) {
		    case TSTR:
		    case TSET:
		    case TREC:
		    case TFILE:
		    case TARY:
			calltype = temptype = P2STRTY;
			p_type_align = align( p -> type );
			break;
		    default:
			if ( p -> class == FFUNC ) {
			    calltype = temptype = p2type( p -> type );
			}
			break;
		}
		if ( temptype != P2UNDEF ) {
		    tempoffset = tmpalloc(p_type_width, p -> type, NOREG);
			/*
			 *	temp
			 *	for (temp = ...
			 */
		    putRV( 0 , cbn , tempoffset , temptype );
		}
	    }
	    switch ( p -> class ) {
		case FUNC:
		case PROC:
			/*
			 *	... p( ...
			 */
		    {
			char	extname[ BUFSIZ ];
			char	*starthere;
			int	funcbn;
			int	i;

			starthere = &extname[0];
			funcbn = p -> nl_block & 037;
			for ( i = 1 ; i < funcbn ; i++ ) {
			    sprintf( starthere , EXTFORMAT , enclosing[ i ] );
			    starthere += strlen( enclosing[ i ] ) + 1;
			}
			sprintf( starthere , EXTFORMAT , p -> symbol );
			starthere += strlen( p -> symbol ) + 1;
			if ( starthere >= &extname[ BUFSIZ ] ) {
			    panic( "call namelength" );
			}
			putleaf( P2ICON , 0 , 0 , p2type( p ) , extname );
		    }
		    break;
		case FFUNC:
		case FPROC:
			    /*
			     *	... (FCALL( p ))( ...
			     */
		    	putleaf( P2ICON , 0 , 0
			    , ADDTYPE( ADDTYPE( p_p2type , P2FTN ) , P2PTR )
			    , "_FCALL" );
			putRV( 0 , cbn , p -> value[NL_OFFS] , P2PTR|P2STRTY );
			putop( P2CALL , p_p2type );
			break;
		default:
			panic("call class");
	    }
	    noarguments = TRUE;
#	endif PC
	/*
	 * Loop and process each of
	 * arguments to the proc/func.
	 *	... ( ... args ... ) ...
	 */
	if ( p -> class == FUNC || p -> class == PROC ) {
	    for (p1 = p->chain; p1 != NIL; p1 = p1->chain) {
		if (argv == NIL) {
			error("Not enough arguments to %s", p->symbol);
			return (NIL);
		}
		switch (p1->class) {
		    case REF:
			    /*
			     * Var parameter
			     */
			    r = argv[1];
			    if (r != NIL && r[0] != T_VAR) {
				    error("Expression given (variable required) for var parameter %s of %s", p1->symbol, p->symbol);
				    break;
			    }
			    q = lvalue( (int *) argv[1], MOD , LREQ );
			    if (q == NIL)
				    break;
			    if (q != p1->type) {
				    error("Parameter type not identical to type of var parameter %s of %s", p1->symbol, p->symbol);
				    break;
			    }
			    break;
		    case VAR:
			    /*
			     * Value parameter
			     */
#			ifdef OBJ
				q = rvalue(argv[1], p1->type , RREQ );
#			endif OBJ
#			ifdef PC
				    /*
				     * structure arguments require lvalues,
				     * scalars use rvalue.
				     */
				switch( classify( p1 -> type ) ) {
				    case TFILE:
				    case TARY:
				    case TREC:
				    case TSET:
				    case TSTR:
					q = rvalue( argv[1] , p1 -> type , LREQ );
					break;
				    case TINT:
				    case TSCAL:
				    case TBOOL:
				    case TCHAR:
					precheck( p1 -> type , "_RANG4" , "_RSNG4" );
					q = rvalue( argv[1] , p1 -> type , RREQ );
					postcheck( p1 -> type );
					break;
				    default:
					q = rvalue( argv[1] , p1 -> type , RREQ );
					if (  isa( p1 -> type  , "d" )
					   && isa( q , "i" ) ) {
					    putop( P2SCONV , P2DOUBLE );
					}
					break;
				}
#			endif PC
			    if (q == NIL)
				    break;
			    if (incompat(q, p1->type, argv[1])) {
				    cerror("Expression type clashed with type of value parameter %s of %s", p1->symbol, p->symbol);
				    break;
			    }
#			ifdef OBJ
				if (isa(p1->type, "bcsi"))
					rangechk(p1->type, q);
				if (q->class != STR)
					convert(q, p1->type);
#			endif OBJ
#			ifdef PC
				switch( classify( p1 -> type ) ) {
				    case TFILE:
				    case TARY:
				    case TREC:
				    case TSET:
				    case TSTR:
					    putstrop( P2STARG
						, p2type( p1 -> type )
						, lwidth( p1 -> type )
						, align( p1 -> type ) );
				}
#			endif PC
			    break;
		    case FFUNC:
			    /*
			     * function parameter
			     */
			    q = flvalue( (int *) argv[1] , FFUNC );
			    if (q == NIL)
				    break;
			    if (q != p1->type) {
				    error("Function type not identical to type of function parameter %s of %s", p1->symbol, p->symbol);
				    break;
			    }
			    break;
		    case FPROC:
			    /*
			     * procedure parameter
			     */
			    q = flvalue( (int *) argv[1] , FPROC );
			    if (q != NIL) {
				    error("Procedure parameter %s of %s cannot have a type", p1->symbol, p->symbol);
			    }
			    break;
		    default:
			    panic("call");
		}
#	    ifdef PC
			/*
			 *	if this is the nth (>1) argument,
			 *	hang it on the left linear list of arguments
			 */
		    if ( noarguments ) {
			    noarguments = FALSE;
		    } else {
			    putop( P2LISTOP , P2INT );
		    }
#	    endif PC
		argv = argv[2];
	    }
	    if (argv != NIL) {
		    error("Too many arguments to %s", p->symbol);
		    rvlist(argv);
		    return (NIL);
	    }
	} else if ( p -> class == FFUNC || p -> class == FPROC ) {
		/*
		 *	formal routines can only have by-value parameters.
		 *	this will lose for integer actuals passed to real
		 *	formals, and strings which people want blank padded.
		 */
#	    ifdef OBJ
		cnt = 0;
#	    endif OBJ
	    for ( ; argv != NIL ; argv = argv[2] ) {
#		ifdef OBJ
		    q = rvalue(argv[1], NIL, RREQ );
		    cnt += leven(lwidth(q));
#		endif OBJ
#		ifdef PC
			/*
			 * structure arguments require lvalues,
			 * scalars use rvalue.
			 */
		    codeoff();
		    p1 = rvalue( argv[1] , NIL , RREQ );
		    codeon();
		    switch( classify( p1 ) ) {
			case TSTR:
			    if ( p1 -> class == STR && slenline != line ) {
				slenline = line;
				( opt( 's' ) ? (standard()): (warning()) );
				error("Implementation can't construct equal length strings");
			    }
			    /* and fall through */
			case TFILE:
			case TARY:
			case TREC:
			case TSET:
			    q = rvalue( argv[1] , p1 , LREQ );
			    break;
			case TINT:
			    if ( floatline != line ) {
				floatline = line;
				( opt( 's' ) ? (standard()) : (warning()) );
				error("Implementation can't coerice integer to real");
			    }
			    /* and fall through */
			case TSCAL:
			case TBOOL:
			case TCHAR:
			default:
			    q = rvalue( argv[1] , p1 , RREQ );
			    break;
		    }
		    switch( classify( p1 ) ) {
			case TFILE:
			case TARY:
			case TREC:
			case TSET:
			case TSTR:
				putstrop( P2STARG , p2type( p1 ) ,
				    lwidth( p1 ) , align( p1 ) );
		    }
			/*
			 *	if this is the nth (>1) argument,
			 *	hang it on the left linear list of arguments
			 */
		    if ( noarguments ) {
			    noarguments = FALSE;
		    } else {
			    putop( P2LISTOP , P2INT );
		    }
#		endif PC
	    }
	} else {
	    panic("call class");
	}
#	ifdef OBJ
	    if ( p -> class == FFUNC || p -> class == FPROC ) {
		put(2, PTR_RV | cbn << 8+INDX, (int)p->value[NL_OFFS]);
		put(2, O_FCALL, (long)cnt);
		put(2, O_FRTN, even(width(p->type)));
	    } else {
		/* put(2, O_CALL | psbn << 8+INDX, (long)p->entloc); */
		put(2, O_CALL | psbn << 8, (long)p->entloc);
	    }
#	endif OBJ
#	ifdef PC
		/*
		 *	do the actual call:
		 *	    either	... p( ... ) ...
		 *	    or		... ( ...() )( ... ) ...
		 *	and maybe an assignment.
		 */
	    if ( porf == FUNC ) {
		switch ( p_type_class ) {
		    case TBOOL:
		    case TCHAR:
		    case TINT:
		    case TSCAL:
		    case TDOUBLE:
		    case TPTR:
			putop( ( noarguments ? P2UNARY P2CALL : P2CALL ) ,
				p_type_p2type );
			if ( p -> class == FFUNC ) {
			    putop( P2ASSIGN , p_type_p2type );
			}
			break;
		    default:
			putstrop( ( noarguments ? P2UNARY P2STCALL : P2STCALL ),
				ADDTYPE( p_type_p2type , P2PTR ) ,
				p_type_width , p_type_align );
			putstrop( P2STASG , p_type_p2type , lwidth( p -> type )
				, align( p -> type ) );
			break;
		}
	    } else {
		putop( ( noarguments ? P2UNARY P2CALL : P2CALL ) , P2INT );
	    }
		/*
		 *	... , FRTN( p ) ...
		 */
	    if ( p -> class == FFUNC || p -> class == FPROC ) {
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR ) ,
			"_FRTN" );
		putRV( 0 , cbn , p -> value[ NL_OFFS ] , P2PTR | P2STRTY );
		putop( P2CALL , P2INT );
		putop( P2COMOP , P2INT );
	    }
		/*
		 *	if required:
		 *	either	... , temp )
		 *	or	... , &temp )
		 */
	    if ( porf == FUNC && temptype != P2UNDEF ) {
		if ( temptype != P2STRTY ) {
		    putRV( 0 , cbn , tempoffset , p_type_p2type );
		} else {
		    putLV( 0 , cbn , tempoffset , p_type_p2type );
		}
		putop( P2COMOP , P2INT );
	    }
	    if ( porf == PROC ) {
		putdot( filename , line );
	    }
#	endif PC
	return (p->type);
}

rvlist(al)
	register int *al;
{

	for (; al != NIL; al = al[2])
		rvalue( (int *) al[1], NLNIL , RREQ );
}
