/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)forop.c 1.2 %G%";

#include	"whoami.h"
#include	"0.h"
#include	"opcode.h"
#include	"tree.h"
#include	"objfmt.h"
#ifdef PC
#    include	"pc.h"
#    include	"pcops.h"
#endif PC
    /*
     *	forop for pc:
     *	    this evaluates the initial and termination expressions,
     *	    checks them to see if the loop executes at all, and then
     *	    does the assignment and the loop.
     *	arg here looks like:
     *	arg[0]	T_FORU or T_FORD
     *	   [1]	lineof "for"
     *	   [2]	[0]	T_ASGN
     *		[1]	lineof ":="
     *		[2]	[0]	T_VAR
     *			[1]	lineof id
     *			[2]	char * to id
     *			[3]	qualifications
     *		[3]	initial expression
     *	  [3]	termination expression
     *	  [4]	statement
     */
forop( arg )
    int	*arg;
    {
	int		*lhs;
	struct nl	*forvar;
	struct nl	*fortype;
	int		forctype;
	int		*init;
	struct nl	*inittype;
	int		initoff;
	int		*term;
	struct nl	*termtype;
	int		termoff;
	int		*stat;
	int		goc;		/* saved gocnt */
	int		again;		/* label at the top of the loop */
	int		after;		/* label after the end of the loop */

	goc = gocnt;
	forvar = NIL;
	if ( arg == NIL ) {
	    goto byebye;
	}
	if ( arg[2] == NIL ) {
	    goto byebye;
	}
	line = arg[1];
	putline();
	lhs = ( (int *) arg[2] )[2];
	init = ( (int *) arg[2] )[3];
	term = arg[3];
	stat = arg[4];
	if ( lhs[3] != NIL ) {
	    error("For variable must be unqualified");
	    rvalue( init , NIL , RREQ );
	    rvalue( term , NIL , RREQ );
	    statement( stat );
	    goto byebye;
	}
	    /*
	     * and this marks the variable as used!!!
	     */
	forvar = lookup( lhs[2] );
	if ( forvar == NIL ) {
	    rvalue( init , NIL , RREQ );
	    rvalue( term , NIL , RREQ );
	    statement( stat );
	    goto byebye;
	}
	    /*
	     * find out the type of the loop variable
	     */
	codeoff();
	fortype = lvalue( lhs , MOD , RREQ );
	codeon();
	    /*
	     * mark the forvar so we can't change it during the loop
	     */
	forvar -> value[ NL_FORV ] = 1;
	if ( fortype == NIL ) {
	    rvalue( init , NIL , RREQ );
	    rvalue( term , NIL , RREQ );
	    statement( stat );
	    goto byebye;
	}
	if ( isnta( fortype , "bcis" ) ) {
	    error("For variables cannot be %ss" , nameof( fortype ) );
	    rvalue( init , NIL , RREQ );
	    rvalue( term , NIL , RREQ );
	    statement( stat );
	    goto byebye;
	}
	    /*
	     * allocate space for the initial and termination expressions
	     */
	sizes[cbn].om_off -= sizeof( long );
	initoff = sizes[cbn].om_off;
	sizes[cbn].om_off -= sizeof( long );
	termoff = sizes[cbn].om_off;
	if ( sizes[cbn].om_off < sizes[cbn].om_max ) {
	    sizes[cbn].om_max = sizes[cbn].om_off;
	}
#	ifdef PC
	    putlbracket( ftnno , -sizes[cbn].om_off );
		/*
		 * compute and save the initial expression
		 */
	    forctype = p2type( fortype );
	    putRV( 0 , cbn , initoff , forctype );
#	endif PC
#	ifdef OBJ
	    put(2, O_LV | cbn<<8+INDX, initoff);
#	endif OBJ
	inittype = rvalue( init , fortype , RREQ );
	if ( incompat( inittype , fortype , init ) ) {
	    cerror("Type of initial expression clashed with index type in 'for' statement");
	    rvalue( term , NIL , RREQ );
	    statement( stat );
	    goto byebye;
	}
#	ifdef PC
	    putop( P2ASSIGN , forctype );
	    putdot( filename , line );
		/*
		 * compute and save the termination expression
		 */
	    putRV( 0 , cbn , termoff , forctype );
#	endif PC
#	ifdef OBJ
	    put(1, width(inittype) <= 2 ? O_AS24 : O_AS4);
		/*
		 * compute and save the termination expression
		 */
	    put(2, O_LV | cbn<<8+INDX, termoff);
#	endif OBJ
	termtype = rvalue( term , fortype , RREQ );
	if ( incompat( termtype , fortype , term ) ) {
	    cerror("Type of limit expression clashed with index type in 'for' statement");
	    statement( stat );
	    goto byebye;
	}
#	ifdef PC
	    putop( P2ASSIGN , forctype );
	    putdot( filename , line );
		/*
		 * we can skip the loop altogether if !( init <= term )
		 */
	    after = getlab();
	    putRV( 0 , cbn , initoff , forctype );
	    putRV( 0 , cbn , termoff , forctype );
	    putop( ( arg[0] == T_FORU ? P2LE : P2GE ) , forctype );
	    putleaf( P2ICON , after , 0 , P2INT , 0 );
	    putop( P2CBRANCH , P2INT );
	    putdot( filename , line );
		/*
		 * okay, then we have to execute the body, but first,
		 * assign the initial expression to the for variable.
		 * see the note in asgnop1 about why this is an rvalue.
		 */
	    rvalue( lhs , NIL , RREQ );
	    if ( opt( 't' ) ) {
		precheck( fortype , "_RANG4" , "_RSNG4" );
	    }
	    putRV( 0 , cbn , initoff , forctype );
	    if ( opt( 't' ) ) {
		postcheck( fortype );
	    }
	    putop( P2ASSIGN , forctype );
	    putdot( filename , line );
#	endif PC
#	ifdef OBJ
	    put(1, width(termtype) <= 2 ? O_AS24 : O_AS4);
		/*
		 * we can skip the loop altogether if !( init <= term )
		 */
	    put(2, O_RV4 | cbn<<8+INDX, initoff);
	    put(2, O_RV4 | cbn<<8+INDX, termoff);
	    gen(NIL, arg[0] == T_FORU ? T_LE : T_GE, sizeof(long),
			sizeof(long));
	    after = getlab();
	    put(2, O_IF, after);
		/*
		 * okay, then we have to execute the body, but first,
		 * assign the initial expression to the for variable.
		 */
	    lvalue( lhs , NOUSE , LREQ );
	    put(2, O_RV4 | cbn<<8+INDX, initoff);
	    rangechk(fortype, nl+T4INT);
	    put(1, width(fortype) <= 2 ? O_AS42 : O_AS4);
#	endif OBJ
	/*
	 * put down the label at the top of the loop
	 */
	again = getlab();
	putlab( again );
	putcnt();
	    /*
	     * and don't forget ...
	     */
	statement( arg[ 4 ] );
	    /*
	     * wasn't that fun?  do we get to do it again?
	     *	we don't do it again if ( !( forvar < limit ) )
	     *	pretend we were doing this at the top of the loop
	     */
	line = arg[ 1 ];
#	ifdef PC
	    if ( opt( 'p' ) ) {
		if ( opt('t') ) {
		    putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			    , "_LINO" );
		    putop( P2UNARY P2CALL , P2INT );
		    putdot( filename , line );
		} else {
		    putRV( STMTCOUNT , 0 , 0 , P2INT );
		    putleaf( P2ICON , 1 , 0 , P2INT , 0 );
		    putop( P2ASG P2PLUS , P2INT );
		    putdot( filename , line );
		}
	    }
	    rvalue( lhs , NIL , RREQ );
	    putRV( 0 , cbn , termoff , forctype );
	    putop( ( arg[ 0 ] == T_FORU ? P2LT : P2GT ) , forctype );
	    putleaf( P2ICON , after , 0 , P2INT , 0 );
	    putop( P2CBRANCH , P2INT );
	    putdot( filename , line );
		/*
		 * okay, so we have to do it again,
		 * but first, increment the for variable.
		 * there it is again, an rvalue on the lhs of an assignment.
		 */
	    rvalue( lhs , NIL , RREQ );
	    if ( opt( 't' ) ) {
		precheck( fortype , "_RANG4" , "_RSNG4" );
	    }
	    rvalue( lhs , NIL , RREQ );
	    putleaf( P2ICON , 1 , 0 , forctype , 0 );
	    putop( ( arg[0] == T_FORU ? P2PLUS : P2MINUS ) , forctype );
	    if ( opt( 't' ) ) {
		postcheck( fortype );
	    }
	    putop( P2ASSIGN , forctype );
	    putdot( filename , line );
		/*
		 * and do it all again
		 */
	    putjbr( again );
		/*
		 * and here we are
		 */
	    putlab( after );
		/*
		 * deallocate the initial and limit variables
		 */
	    sizes[cbn].om_off += 2 * ( sizeof( long ) );
	    putlbracket( ftnno , -sizes[cbn].om_off );
#	endif PC
#	ifdef OBJ
		/*
		 * okay, so we have to do it again.
		 * Luckily we have a magic opcode which increments the
		 * index variable, checks the limit falling through if
		 * it has been reached, else range checking the result
		 * updating the index variable, and returning to the top
		 * of the loop.
		 */
	    putline();
	    put(2, O_RV4 | cbn<<8+INDX, termoff);
	    lvalue(lhs, MOD, LREQ);
	    put(4, (arg[0] == T_FORU ? O_FOR1U : O_FOR1D) + (width(fortype)>>1),
		    fortype->range[0], fortype->range[1], again);
		/*
		 * and here we are
		 */
	    patch( after );
		/*
		 * deallocate the initial and limit variables
		 */
	    sizes[cbn].om_off += 2 * ( sizeof( long ) );
#	endif OBJ
byebye:
	noreach = 0;
	if ( forvar != NIL ) {
	    forvar -> value[ NL_FORV ] = 0;
	}
	if ( goc != gocnt ) {
	    putcnt();
	}
    }
