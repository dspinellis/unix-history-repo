/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)forop.c 1.5 %G%";

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
	if (lhs == NIL) {
nogood:
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
	    goto nogood;
	}
	if ( lhs[3] != NIL ) {
	    error("For variable %s must be unqualified", forvar->symbol);
	    goto nogood;
	}
	if (forvar->class == WITHPTR) {
	    error("For variable %s cannot be an element of a record", lhs[2]);
	    goto nogood;
	}
	if (opt('s') &&
	    (bn != cbn || whereis(forvar->value[NL_OFFS]) == PARAMVAR)) {
	    standard();
	    error("For variable %s must be declared in the block in which it is used", forvar->symbol);
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
	if (forvar->value[NL_FORV]) {
	    error("Can't modify the for variable %s in the range of the loop", forvar->symbol);
	    forvar = NIL;
	    goto nogood;
	}
	forvar -> value[ NL_FORV ] = 1;
	if ( fortype == NIL ) {
	    goto nogood;
	}
	if ( isnta( fortype , "bcis" ) ) {
	    error("For variable %s cannot be %ss", forvar->symbol, nameof( fortype ) );
	    goto nogood;
	}
	    /*
	     * allocate space for the initial and termination expressions
	     */
	initoff = tmpalloc(sizeof(long), nl+T4INT, REGOK);
	forvar -> value[ NL_SOFFS ] = forvar -> value[ NL_OFFS ];
	forvar -> value[ NL_OFFS ] = initoff;
	forvar -> value[ NL_FORV ] = 3;
	termoff = tmpalloc(sizeof(long), nl+T4INT, REGOK);
#	ifdef PC
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
	    gen(O_AS2, O_AS2, sizeof(long), width(inittype));
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
		 * put down the label at the top of the loop
		 */
	    again = getlab();
	    putlab( again );
		/*
		 * okay, then we have to execute the body, but first,
		 * assign the initial expression to the for variable.
		 * see the note in asgnop1 about why this is an rvalue.
		 */
	    forvar -> value[ NL_OFFS ] = forvar -> value[ NL_SOFFS ];
	    rvalue( lhs , NIL , RREQ );
	    forvar -> value[ NL_OFFS ] = initoff;
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
	    gen(O_AS2, O_AS2, sizeof(long), width(termtype));
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
		 * put down the label at the top of the loop
		 */
	    again = getlab();
	    putlab( again );
		/*
		 * okay, then we have to execute the body, but first,
		 * assign the initial expression to the for variable.
		 */
	    forvar -> value[ NL_OFFS ] = forvar -> value[ NL_SOFFS ];
	    lvalue( lhs , NOUSE , LREQ );
	    put(2, O_RV4 | cbn<<8+INDX, initoff);
	    rangechk(fortype, nl+T4INT);
	    gen(O_AS2, O_AS2, width(fortype), sizeof(long));
	    forvar -> value[ NL_OFFS ] = initoff;
#	endif OBJ
	    /*
	     * and don't forget ...
	     */
	putcnt();
	statement( stat );
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
	    if (width(fortype) <= 2)
		    put(4, (arg[0] == T_FORU ? O_FOR1U : O_FOR1D) +
			    (width(fortype)>>1), (int)fortype->range[0],
			    (int)fortype->range[1], again);
	    else
		    put(4, (arg[0] == T_FORU ? O_FOR4U : O_FOR4D),
			    fortype->range[0], fortype->range[1], again);
		/*
		 * and here we are
		 */
	    patch( after );
#	endif OBJ
byebye:
	noreach = 0;
	if ( forvar != NIL ) {
	    if (forvar -> value[ NL_FORV ] > 1)
		forvar -> value[ NL_OFFS ] = forvar -> value[ NL_SOFFS ];
	    forvar -> value[ NL_FORV ] = 0;
	}
	if ( goc != gocnt ) {
	    putcnt();
	}
    }
