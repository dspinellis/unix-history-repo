/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)call.c 1.3 10/2/80";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
#   include "pc.h"
#   include "pcops.h"
#endif PC

bool	slenflag = 0;
bool	floatflag = 0;

/*
 * Call generates code for calls to
 * user defined procedures and functions
 * and is called by proc and funccod.
 * P is the result of the lookup
 * of the procedure/function symbol,
 * and porf is PROC or FUNC.
 * Psbn is the block number of p.
 */
struct nl *
call(p, argv, porf, psbn)
	struct nl *p;
	int *argv, porf, psbn;
{
	register struct nl *p1, *q;
	int *r;

#	ifdef OBJ
	    int		cnt;
#	endif OBJ
#	ifdef PC
	    long	temp;
	    int		firsttime;
	    int		rettype;
#	endif PC

#	ifdef OBJ
	    if (p->class == FFUNC || p->class == FPROC)
		put(2, PTR_RV | cbn << 8+INDX, p->value[NL_OFFS]);
	    if (porf == FUNC)
		    /*
		     * Push some space
		     * for the function return type
		     */
		    put2(O_PUSH, even(-width(p->type)));
#	endif OBJ
#	ifdef PC
	    if ( porf == FUNC ) {
		switch( classify( p -> type ) ) {
		    case TSTR:
		    case TSET:
		    case TREC:
		    case TFILE:
		    case TARY:
			temp = sizes[ cbn ].om_off -= width( p -> type );
			putlbracket( ftnno , -sizes[cbn].om_off );
			if (sizes[cbn].om_off < sizes[cbn].om_max) {
				sizes[cbn].om_max = sizes[cbn].om_off;
			}
			putRV( 0 , cbn , temp , P2STRTY );
		}
	    }
	    switch ( p -> class ) {
		case FUNC:
		case PROC:
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
			     *	start one of these:
			     *	FRTN( frtn , ( *FCALL( frtn ) )(...args...) )
			     */
			putleaf( P2ICON , 0 , 0 , p2type( p ) , "_FRTN" );
			putRV( 0 , cbn , p -> value[NL_OFFS] , P2PTR|P2STRTY );
		    	putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2PTR , ADDTYPE( P2FTN , p2type( p ) ) )
			    , "_FCALL" );
			putRV( 0 , cbn , p -> value[NL_OFFS] , P2PTR|P2STRTY );
			putop( P2CALL , p2type( p ) );
			break;
		default:
			panic("call class");
	    }
	    firsttime = TRUE;
#	endif PC
	/*
	 * Loop and process each of
	 * arguments to the proc/func.
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
		    if ( firsttime ) {
			    firsttime = FALSE;
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
		    cnt += even(lwidth(q));
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
			    if ( p1 -> class == STR && slenflag == 0 ) {
				if ( opt( 's' ) ) {
				    standard();
				} else {
				    warning();
				}
				error("Implementation can't construct equal length strings");
				slenflag++;
			    }
			    /* and fall through */
			case TFILE:
			case TARY:
			case TREC:
			case TSET:
			    q = rvalue( argv[1] , p1 , LREQ );
			    break;
			case TINT:
			    if ( floatflag == 0 ) {
				if ( opt( 's' ) ) {
				    standard();
				} else {
				    warning();
				}
				error("Implementation can't coerice integer to real");
				floatflag++;
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
		    if ( firsttime ) {
			    firsttime = FALSE;
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
		put(2, PTR_RV | cbn << 8+INDX, p->value[NL_OFFS]);
		put(2, O_FCALL, cnt);
		put(2, O_FRTN, even(lwidth(p->type)));
	    } else {
		put2(O_CALL | psbn << 8+INDX, p->entloc);
	    }
#	endif OBJ
#	ifdef PC
	    if ( porf == FUNC ) {
		rettype = p2type( p -> type );
		switch ( classify( p -> type ) ) {
		    case TBOOL:
		    case TCHAR:
		    case TINT:
		    case TSCAL:
		    case TDOUBLE:
		    case TPTR:
			if ( firsttime ) {
				putop( P2UNARY P2CALL , rettype );
			} else {
				putop( P2CALL , rettype );
			}
			if (p -> class == FFUNC || p -> class == FPROC ) {
			    putop( P2LISTOP , P2INT );
			    putop( P2CALL , rettype );
			}
			break;
		    default:
			if ( firsttime ) {
				putstrop( P2UNARY P2STCALL
					, ADDTYPE( rettype , P2PTR )
					, lwidth( p -> type )
					, align( p -> type ) );
			} else {
				putstrop( P2STCALL
					, ADDTYPE( rettype , P2PTR )
					, lwidth( p -> type )
					, align( p -> type ) );
			}
			if (p -> class == FFUNC || p -> class == FPROC ) {
			    putop( P2LISTOP , P2INT );
			    putop( P2CALL , ADDTYPE( rettype , P2PTR ) );
			}
			putstrop( P2STASG , rettype , lwidth( p -> type )
				, align( p -> type ) );
			putLV( 0 , cbn , temp , rettype );
			putop( P2COMOP , P2INT );
			break;
		}
	    } else {
		if ( firsttime ) {
			putop( P2UNARY P2CALL , P2INT );
		} else {
			putop( P2CALL , P2INT );
		}
		if (p -> class == FFUNC || p -> class == FPROC ) {
		    putop( P2LISTOP , P2INT );
		    putop( P2CALL , P2INT );
		}
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
