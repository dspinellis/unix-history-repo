/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)pccaseop.c 1.1 %G%";

#include "whoami.h"
#ifdef PC
    /*
     *	and the rest of the file
     */
#include "0.h"
#include "tree.h"
#include "objfmt.h"
#include "pcops.h"
#include "pc.h"
    /*
     *	tcase	[0]	T_CASE
     *		[1]	lineof "case"
     *		[2]	expression
     *		[3]	list of cased statements
     *			cstat	[0]	T_CSTAT
     *				[1]	lineof ":"
     *				[2]	constant list
     *				[3]	statement
     */

struct ct {
    long	clong;
    int		cline;
};

pccaseop( tcase )
    int	*tcase;
    {
	struct nl	*exprtype;
	struct nl	*rangetype;
	long		low;
	long		high;
	long		exproff;
	long		exprctype;
	long		count;
	long		*cstatlp;
	long		*cstatp;
	struct ct	*ctab;
	long		endlabel;
	long		nextlabel;
	long		firsttime;
	long		*casep;
	long		i;
	long		nr;
	long		goc;

	goc = gocnt;
	    /*
	     *	find out the type of the case expression
	     */
	line = tcase[1];
	codeoff();
	exprtype = rvalue( (int *) tcase[2] , NIL  , RREQ );
	codeon();
	if ( exprtype != NIL ) {
	    if ( isnta( exprtype , "bcsi" ) ) {
		error("Case selectors cannot be %ss" , nameof( exprtype ) );
		exprtype = NIL;
	    } else {
		if ( exprtype -> class != RANGE ) {
		    rangetype = exprtype -> type;
		} else {
		    rangetype = exprtype;
		}
		if ( rangetype == NIL ) {
		    exprtype = NIL;
		} else {
		    low = rangetype -> range[0];
		    high = rangetype -> range[1];
		}
	    }
	}
	if ( exprtype != NIL ) {
		/*
		 * allocate temporary for case expression
		 */
	    sizes[cbn].om_off -= sizeof( long );
	    exproff = sizes[cbn].om_off;
	    putlbracket( ftnno , -sizes[cbn].om_off );
	    if ( sizes[cbn].om_off < sizes[cbn].om_max ) {
		sizes[cbn].om_max = sizes[cbn].om_off;
	    }
		/*
		 * compute and save the expression
		 */
	    exprctype = p2type( exprtype );
	    putRV( 0 , cbn , exproff , P2INT );
	    rvalue( (int *) tcase[2] , NIL  , RREQ );
	    putop( P2ASSIGN , exprctype );
	    putdot( filename , line );
	}
	    /*
	     *	count the number of cases
	     *	and allocate table for cases and lines
	     */
	count = 0;
	for ( cstatlp = tcase[3] ; cstatlp != NIL ; cstatlp = cstatlp[2] ) {
	    cstatp = cstatlp[1];
	    if ( cstatp == NIL ) {
		continue;
	    }
	    for ( casep = cstatp[2] ; casep != NIL ; casep = casep[2] ) {
		count++;
	    }
	}
	ctab = (struct ct *) malloc( count * sizeof( struct ct ) );
	if ( ctab == (struct ct *) -1 ) {
	    error("Ran out of memory (case)");
	    pexit( DIED );
	}
	    /*
	     * generate code for each case
	     */
	endlabel = getlab();
	nextlabel = getlab();
	count = 0;
	nr = 1;
	for ( cstatlp = tcase[3] ; cstatlp != NIL ; cstatlp = cstatlp[2] ) {
	    cstatp = cstatlp[1];
	    if ( cstatp == NIL ) {
		continue;
	    }
	    line = cstatp[1];
	    putlab( nextlabel );
	    nextlabel = getlab();
		/*
		 * if it's not any of these, then go to next
		 */
	    firsttime = 1;
	    for ( casep = cstatp[2] ; casep != NIL ; casep = casep[2] ) {
		gconst( casep[1] );
		if( exprtype == NIL || con.ctype == NIL ) {
		    continue;
		}
		if ( incompat( con.ctype , exprtype , NIL ) ) {
		    cerror("Case label type clashed with case selector expression type");
		    continue;
		}
		if ( con.crval < low || con.crval > high ) {
		    error("Case label out of range");
		    continue;
		}
		ctab[ count ].clong = con.crval;
		ctab[ count ].cline = line;
		    /*
		     *	check for duplicates
		     */
		for ( i = 0 ; i < count ; i++ ) {
		    if ( ctab[ i ].clong == con.crval ) {
			error("Multiply defined label in case, lines %d and %d"
				, ctab[ i ].cline , line );
		    }
		}
		putRV( 0 , cbn , exproff , exprctype , 0 );
		putleaf( P2ICON , ctab[ count ].clong , 0 , exprctype , 0 );
		putop( P2EQ , exprctype );
		if ( ! firsttime ) {
			/*
			 * note the use of !! to get short circuiting
			 */
		    putop( P2OROR , P2CHAR );
		}
		firsttime = 0;
	    }
	    putleaf( P2ICON , nextlabel , 0 , P2INT , 0 );
	    putop( P2CBRANCH , P2INT );
	    putdot( filename , line );
		/*
		 * if we get here, we must be in this case
		 */
	    putcnt();
	    level++;
	    statement( cstatp[3] );
	    nr &= noreach;
	    noreach = 0;
	    putjbr( endlabel );
	    level--;
	    if (gotos[cbn]) {
		    ungoto();
	    }
	}
	    /*
	     *	default action is to call error
	     */
	putlab( nextlabel );
	putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_ERROR" );
	putleaf( P2ICON , ECASE , 0 , P2INT , 0 );
	putRV( 0 , cbn , exproff , P2INT );
	putop( P2LISTOP , P2INT );
	putop( P2CALL , P2INT );
	putdot( filename , line );
	putlab( endlabel );
	noreach = nr;
	if ( goc != gocnt ) {
		putcnt();
	}
    }

#endif PC
