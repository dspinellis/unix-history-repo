/* Copyright (c) 1980 Regents of the University of California */

static	char sccsid[] = "@(#)pccaseop.c 1.8 %G%";

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
     *	structure for a case: 
     *	    its constant label, line number (for errors), and location label.
     */
struct ct {
    long	cconst;
    int		cline;
    int		clabel;
};

    /*
     *	the P2FORCE operator puts its operand into a register.
     *	these to keep from thinking of it as r0 all over.
     */
#define	FORCENAME	"r0"

    /*
     *	given a tree for a case statement, generate code for it.
     *	this computes the expression into a register,
     *	puts down the code for each of the cases,
     *	and then decides how to do the case switching.
     *	tcase	[0]	T_CASE
     *		[1]	lineof "case"
     *		[2]	expression
     *		[3]	list of cased statements:
     *			cstat	[0]	T_CSTAT
     *				[1]	lineof ":"
     *				[2]	list of constant labels
     *				[3]	statement
     */
pccaseop( tcase )
    int	*tcase;
{
    struct nl	*exprtype;
    struct nl	*exprnlp;
    struct nl	*rangetype;
    long	low;
    long	high;
    long	exprctype;
    long	swlabel;
    long	endlabel;
    long	label;
    long	count;
    long	*cstatlp;
    long	*cstatp;
    long	*casep;
    struct ct	*ctab;
    struct ct	*ctp;
    long	i;
    long	nr;
    long	goc;
    int		casecmp();
    bool	dupcases;

    goc = gocnt;
	/*
	 *  find out the type of the case expression
	 *  even if the expression has errors (exprtype == NIL), continue.
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
	     *	compute and save the case expression.
	     *	also, put expression into a register
	     *	save its c-type and jump to the code to do the switch.
	     */
	exprctype = p2type( exprtype );
	exprnlp = tmpalloc( sizeof (long) , nl + T4INT , NOREG );
	putRV( 0 , cbn , exprnlp -> value[ NL_OFFS ] ,
			exprnlp -> extra_flags , P2INT );
	(void) rvalue( (int *) tcase[2] , NIL , RREQ );
	putop( P2ASSIGN , P2INT );
	putop( P2FORCE , P2INT );
	putdot( filename , line );
	swlabel = getlab();
	putjbr( swlabel );
    }
	/*
	 *  count the number of cases
	 *  and allocate table for cases, lines, and labels
	 *  default case goes in ctab[0].
	 */
    count = 1;
    for ( cstatlp = tcase[3] ; cstatlp != NIL ; cstatlp = cstatlp[2] ) {
	cstatp = cstatlp[1];
	if ( cstatp == NIL ) {
	    continue;
	}
	for ( casep = cstatp[2] ; casep != NIL ; casep = casep[2] ) {
	    count++;
	}
    }
	/*
	 */
    ctab = (struct ct *) malloc( count * sizeof( struct ct ) );
    if ( ctab == (struct ct *) 0 ) {
	error("Ran out of memory (case)");
	pexit( DIED );
    }
	/*
	 *  pick up default label and label for after case statement.
	 */
    ctab[0].clabel = getlab();
    endlabel = getlab();
	/*
	 *  generate code for each case
	 *  filling in ctab for each.
	 *  nr is for error if no case falls out bottom.
	 */
    nr = 1;
    count = 0;
    for ( cstatlp = tcase[3] ; cstatlp != NIL ; cstatlp = cstatlp[2] ) {
	cstatp = cstatlp[1];
	if ( cstatp == NIL ) {
	    continue;
	}
	line = cstatp[1];
	label = getlab();
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
	    count++;
	    ctab[ count ].cconst = con.crval;
	    ctab[ count ].cline = line;
	    ctab[ count ].clabel = label;
	}
	    /*
	     *	put out the statement
	     */
	putlab( label );
	putcnt();
	level++;
	statement( cstatp[3] );
	nr = (nr && noreach);
	noreach = 0;
	level--;
	if (gotos[cbn]) {
		ungoto();
	}
	putjbr( endlabel );
    }
    noreach = nr;
	/*
	 *	default action is to call error
	 */
    putlab( ctab[0].clabel );
    putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_CASERNG" );
    putRV( 0 , cbn , exprnlp -> value[ NL_OFFS ] ,
		    exprnlp -> extra_flags , P2INT );
    putop( P2CALL , P2INT );
    putdot( filename , line );
	/*
	 *  sort the cases
	 */
    qsort( &ctab[1] , count , sizeof (struct ct) , casecmp );
	/*
	 *  check for duplicates
	 */
    dupcases = FALSE;
    for ( ctp = &ctab[1] ; ctp < &ctab[ count ] ; ctp++ ) {
	if ( ctp[0].cconst == ctp[1].cconst ) {
	    error("Multiply defined label in case, lines %d and %d" ,
		    ctp[0].cline , ctp[1].cline );
	    dupcases = TRUE;
	}
    }
    if ( dupcases ) {
	return;
    }
	/*
	 *  choose a switch algorithm and implement it:
	 *	direct switch	>= 1/3 full and >= 4 cases.
	 *	binary switch	not direct switch and > 8 cases.
	 *	ifthenelse	not direct or binary switch.
	 */
    putlab( swlabel );
    if ( ctab[ count ].cconst - ctab[1].cconst < 3 * count && count >= 4 ) {
	directsw( ctab , count );
    } else if ( count > 8 ) {
	binarysw( ctab , count );
    } else {
	itesw( ctab , count );
    }
    putlab( endlabel );
    if ( goc != gocnt ) {
	    putcnt();
    }
}

    /*
     *	direct switch
     */
directsw( ctab , count )
    struct ct	*ctab;
    int		count;
{
    int		fromlabel = getlab();
    long	i;
    long	j;

    putprintf( "	casel	%s,$%d,$%d" , 0 , FORCENAME ,
	    ctab[1].cconst , ctab[ count ].cconst - ctab[1].cconst );
    putlab( fromlabel );
    i = 1;
    j = ctab[1].cconst;
    while ( i <= count ) {
	if ( j == ctab[ i ].cconst ) {
	    putprintf( "	.word	" , 1 );
	    putprintf( PREFIXFORMAT , 1 , LABELPREFIX , ctab[ i ].clabel );
	    putprintf( "-" , 1 );
	    putprintf( PREFIXFORMAT , 0 , LABELPREFIX , fromlabel );
	    i++;
	} else {
	    putprintf( "	.word	" , 1 );
	    putprintf( PREFIXFORMAT , 1 , LABELPREFIX , ctab[ 0 ].clabel );
	    putprintf( "-" , 1 );
	    putprintf( PREFIXFORMAT , 0 , LABELPREFIX , fromlabel );
	}
	j++;
    }
    putjbr( ctab[0].clabel );
}

    /*
     *	binary switch
     *	special case out default label and start recursion.
     */
binarysw( ctab , count )
    struct ct	*ctab;
    int		count;
{
    
    bsrecur( ctab[0].clabel , &ctab[0] , count );
}

    /*
     *	recursive log( count ) search.
     */
bsrecur( deflabel , ctab , count )
    int		deflabel;
    struct ct	*ctab;
    int		count;
{

    if ( count <= 0 ) {
	putprintf( "	jbr	L%d" , 0 , deflabel );
	return;
    } else if ( count == 1 ) {
	putprintf( "	cmpl	%s,$%d" , 0 , FORCENAME , ctab[1].cconst );
	putprintf( "	jeql	L%d" , 0 , ctab[1].clabel );
	putprintf( "	jbr	L%d" , 0 , deflabel );
	return;
    } else {
	int	half = ( count + 1 ) / 2;
	int	gtrlabel = getlab();

	putprintf( "	cmpl	%s,$%d" , 0 , FORCENAME , ctab[ half ].cconst );
	putprintf( "	jgtr	L%d" , 0 , gtrlabel );
	putprintf( "	jeql	L%d" , 0 , ctab[ half ].clabel );
	bsrecur( deflabel , &ctab[0] , half - 1 );
	putprintf( "L%d:" , 0 , gtrlabel );
	bsrecur( deflabel , &ctab[ half ] , count - half );
	return;
    }
}

itesw( ctab , count )
    struct ct	*ctab;
    int		count;
{
    int	i;

    for ( i = 1 ; i <= count ; i++ ) {
	putprintf( "	cmpl	%s,$%d" , 0 , FORCENAME , ctab[ i ].cconst );
	putprintf( "	jeql	L%d" , 0 , ctab[ i ].clabel );
    }
    putprintf( "	jbr	L%d" , 0 , ctab[0].clabel );
    return;
}
int
casecmp( this , that )
    struct ct 	*this;
    struct ct 	*that;
{
    if ( this -> cconst < that -> cconst ) {
	return -1;
    } else if ( this -> cconst > that -> cconst ) {
	return 1;
    } else {
	return 0;
    }
}
#endif PC
