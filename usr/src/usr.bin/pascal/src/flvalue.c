/* Copyright (c) 1980 Regents of the University of California */

static	char sccsid[] = "@(#)flvalue.c 1.1 %G%";

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
     *	flvalue generates the code to either pass on a formal routine,
     *	or construct the structure which is the environment for passing.
     *	it tells the difference by looking at the tree it's given.
     */
struct nl *
flvalue( r )
    int	*r;
    {
	struct nl	*p;
	long		tempoff;

	if ( r == NIL ) {
	    return NIL;
	}
	p = lookup(r[2]);
	if (p == NIL) {
		return NIL;
	}
	switch ( r[0] ) {
	    case T_FFUNC:
		    if ( r[3] != NIL ) {
			error("Formal function %s cannot be qualified" ,
				p -> symbol );
			return NIL;
		    }
		    goto froutine;
	    case T_FPROC:
		    if ( r[3] != NIL ) {
			error("Formal procedure %s cannot be qualified" ,
				p -> symbol );
			return NIL;
		    }
	    froutine:
#		    ifdef OBJ
			put( 2 , PTR_RV | bn << 8+INDX , p -> value[NL_OFFS] );
#		    endif OBJ
#		    ifdef PC
			putRV( p -> symbol , bn , p -> value[ NL_OFFS ] , 
				p2type( p ) );
#		    endif PC
		    return p -> type;
	    case T_FUNC:
		    if ( r[3] != NIL ) {
			error("Function %s cannot be qualified" , p -> symbol );
			return NIL;
		    }
		    goto routine;
	    case T_PROC:
		    if ( r[3] != NIL ) {
			error("Procedure %s cannot be qualified", p -> symbol );
			return NIL;
		    }
	    routine:
			/*
			 *	formal routine structure:
			 *
			 *	struct formalrtn {
			 *		long		(*entryaddr)();
			 *		long		cbn;
			 *		struct dispsave	disp[2*MAXLVL];
			 *	};
			 */
		    sizes[ cbn ].om_off -=	  sizeof (long (*()))
						+ sizeof (long)
						+ 2*bn*sizeof (struct dispsave);
		    tempoff = sizes[ cbn ].om_off;
		    if ( sizes[ cbn ].om_off < sizes[ cbn ].om_max ) {
			sizes[ cbn ].om_max = tempoff;
		    }
#		    ifdef OBJ
			put( 2 , PTR_LV | cbn << 8 + INDX , tempoff );
			put( 2 , O_FSAV | bn << 8 + INDX , p -> entloc );
#		    endif OBJ
#		    ifdef PC
			putlbracket( ftnno , -tempoff );
			putleaf( P2ICON , 0 , 0 ,
			    ADDTYPE( P2PTR , ADDTYPE( P2FTN , P2PTR|P2STR ) ) ,
			    "_FSAV" );
			{
			    char	extname[ BUFSIZ ];
			    char	*starthere;
			    int		i;

			    starthere = &extname[0];
			    for ( i = 1 ; i < bn ; i++ ) {
				sprintf( starthere , EXTFORMAT , enclosing[ i ] );
				starthere += strlen( enclosing[ i ] ) + 1;
			    }
			    sprintf( starthere , EXTFORMAT , p -> symbol );
			    starthere += strlen( p -> symbol ) + 1;
			    if ( starthere >= &extname[ BUFSIZ ] ) {
				panic( "flvalue namelength" );
			    }
			    putleaf( P2ICON , 0 , 0 , p2type( p ) , extname );
			}
			putleaf( P2ICON , bn , 0 , P2INT , 0 );
			putop( P2LISTOP , P2INT );
			putLV( 0 , cbn , tempoff , P2STR );
			putop( P2LISTOP , P2INT );
			putop( P2CALL , P2PTR | P2STRTY );
#		    endif PC
		    return p -> type;
	    default:
		    panic("flvalue");
	}
    }
