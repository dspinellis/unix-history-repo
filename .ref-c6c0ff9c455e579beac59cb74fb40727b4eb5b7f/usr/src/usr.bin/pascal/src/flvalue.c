/* Copyright (c) 1980 Regents of the University of California */

static	char sccsid[] = "@(#)flvalue.c 1.2 %G%";

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
 * define the display structure for purposes of allocating
 * a temporary
 */
struct dispsave {
	char	*ptr;
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
			put( 2 , PTR_RV | bn << 8+INDX , p -> value[NL_OFFS] );
#		    endif OBJ
#		    ifdef PC
			putRV( p -> symbol , bn , p -> value[ NL_OFFS ] , 
				p2type( p ) );
#		    endif PC
		    return p -> type;
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
		    sizes[ cbn ].om_off -=	  sizeof (long (*)())
						+ sizeof (long)
						+ 2*bn*sizeof (struct dispsave);
		    tempoff = sizes[ cbn ].om_off;
		    if ( sizes[ cbn ].om_off < sizes[ cbn ].om_max ) {
			sizes[ cbn ].om_max = tempoff;
		    }
#		    ifdef OBJ
			put( 2 , O_LV | cbn << 8 + INDX , tempoff );
			put( 2 , O_FSAV | bn << 8 + INDX , p -> entloc );
#		    endif OBJ
#		    ifdef PC
			putlbracket( ftnno , -tempoff );
			putleaf( P2ICON , 0 , 0 ,
			    ADDTYPE( P2PTR , ADDTYPE( P2FTN , P2PTR|P2STRTY ) ) ,
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
			putLV( 0 , cbn , tempoff , P2STRTY );
			putop( P2LISTOP , P2INT );
			putop( P2CALL , P2PTR | P2STRTY );
#		    endif PC
		    return p -> type;
	    default:
		    error("Variable given, %s required for %s parameter %s" ,
			    typename , typename , formalp -> symbol );
		    return NIL;
	}
    }
