/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)call.c 1.2 %G%";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
#   include "pc.h"
#   include "pcops.h"
#endif PC

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

#	ifdef PC
	    long	temp;
	    int		firsttime;
	    int		rettype;
#	endif PC

#	ifdef OBJ
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
	    firsttime = TRUE;
#	endif PC
	/*
	 * Loop and process each of
	 * arguments to the proc/func.
	 */
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
#	ifdef OBJ
	    put2(O_CALL | psbn << 8+INDX, p->entloc);
	    put2(O_POP, p->value[NL_OFFS]-DPOFF2);
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
			if ( p -> chain == NIL ) {
				putop( P2UNARY P2CALL , rettype );
			} else {
				putop( P2CALL , rettype );
			}
			break;
		    default:
			if ( p -> chain == NIL ) {
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
			putstrop( P2STASG , rettype , lwidth( p -> type )
				, align( p -> type ) );
			putLV( 0 , cbn , temp , rettype );
			putop( P2COMOP , P2INT );
			break;
		}
	    } else {
		if ( p -> chain == NIL ) {
			putop( P2UNARY P2CALL , P2INT );
		} else {
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
