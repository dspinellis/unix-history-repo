/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)forop.c 1.14 %G%";

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
     *	for-statements.
     *
     *	the relevant quote from the standard:  6.8.3.9:
     *	``The control-variable shall be an entire-variable whose identifier
     *	is declared in the variable-declaration-part of the block closest-
     *	containing the for-statement.  The control-variable shall possess
     *	an ordinal-type, and the initial-value and the final-value shall be
     *	of a type compatible with this type.  The statement of a for-statement
     *	shall not contain an assigning-reference to the control-variable
     *	of the for-statement.  The value of the final-value shall be 
     *	assignment-compatible with the control-variable when the initial-value
     *	is assigned to the control-variable.  After a for-statement is
     *	executed (other than being left by a goto-statement leading out of it)
     *	the control-variable shall be undefined.  Apart from the restrictions
     *	imposed by these requirements, the for-statement
     *		for v := e1 to e2 do body
     *	shall be equivalent to
     *		begin
     *		    temp1 := e1;
     *		    temp2 := e2;
     *		    if temp1 <= temp2 then begin
     *			v := temp1;
     *			body;
     *			while v <> temp2 do begin
     *			    v := succ(v);
     *			    body;
     *			end
     *		    end
     *		end
     *	where temp1 and temp2 denote auxiliary variables that the program
     *	does not otherwise contain, and that possess the type possessed by
     *	the variable v if that type is not a subrange-type;  otherwise the
     *	host type possessed by the variable v.''
     *
     *	The Berkeley Pascal systems try to do all that without duplicating
     *	the body, and shadowing the control-variable in (possibly) a
     *	register variable.
     *
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
#ifdef PC
	int		forp2type;
#endif PC
	int		forwidth;
	int		*init;
	struct nl	*inittype;
	struct nl	*initnlp;	/* initial value namelist entry */
	int		*term;
	struct nl	*termtype;
	struct nl	*termnlp;	/* termination value namelist entry */
	struct nl	*shadownlp;	/* namelist entry for the shadow */
	int		*stat;
	int		goc;		/* saved gocnt */
	int		again;		/* label at the top of the loop */
	int		after;		/* label after the end of the loop */
	struct nl	saved_nl;	/* saved namelist entry for loop var */

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
	    if (forvar != NIL) {
		forvar->value[ NL_FORV ] = FORVAR;
	    }
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
	saved_nl = *forvar;
	if ( lhs[3] != NIL ) {
	    error("For variable %s must be unqualified", forvar->symbol);
	    goto nogood;
	}
	if (forvar->class == WITHPTR) {
	    error("For variable %s cannot be an element of a record", lhs[2]);
	    goto nogood;
	}
	if ( opt('s') &&
	    ( ( bn != cbn ) ||
#ifdef OBJ
		(whereis(bn, forvar->value[NL_OFFS], 0) == PARAMVAR)
#endif OBJ
#ifdef PC
		(whereis(bn, forvar->value[NL_OFFS], forvar->extra_flags)
		    == PARAMVAR )
#endif PC
	    ) ) {
	    standard();
	    error("For variable %s must be declared in the block in which it is used", forvar->symbol);
	}
	    /*
	     * find out the type of the loop variable
	     */
	codeoff();
	fortype = lvalue( lhs , MOD , RREQ );
	codeon();
	if ( fortype == NIL ) {
	    goto nogood;
	}
	if ( isnta( fortype , "bcis" ) ) {
	    error("For variable %s cannot be %ss", forvar->symbol, nameof( fortype ) );
	    goto nogood;
	}
	if ( forvar->value[ NL_FORV ] & FORVAR ) {
	    error("Can't modify the for variable %s in the range of the loop", forvar->symbol);
	    forvar = NIL;
	    goto nogood;
	}
	forwidth = lwidth(fortype);
#	ifdef PC
	    forp2type = p2type(fortype);
#	endif PC
	    /*
	     *	allocate temporaries for the initial and final expressions
	     *	and maybe a register to shadow the for variable.
	     */
	initnlp = tmpalloc(sizeof(long), nl+T4INT, NOREG);
	termnlp = tmpalloc(sizeof(long), nl+T4INT, NOREG);
	shadownlp = tmpalloc(forwidth, fortype, REGOK);
#	ifdef PC
		/*
		 * compute and save the initial expression
		 */
	    putRV( 0 , cbn , initnlp -> value[ NL_OFFS ] ,
		    initnlp -> extra_flags , P2INT );
#	endif PC
#	ifdef OBJ
	    put(2, O_LV | cbn<<8+INDX, initnlp -> value[ NL_OFFS ] );
#	endif OBJ
	inittype = rvalue( init , fortype , RREQ );
	if ( incompat( inittype , fortype , init ) ) {
	    cerror("Type of initial expression clashed with index type in 'for' statement");
	    if (forvar != NIL) {
		forvar->value[ NL_FORV ] = FORVAR;
	    }
	    rvalue( term , NIL , RREQ );
	    statement( stat );
	    goto byebye;
	}
#	ifdef PC
	    sconv(p2type(inittype), P2INT);
	    putop( P2ASSIGN , P2INT );
	    putdot( filename , line );
		/*
		 * compute and save the termination expression
		 */
	    putRV( 0 , cbn , termnlp -> value[ NL_OFFS ] ,
		    termnlp -> extra_flags , P2INT );
#	endif PC
#	ifdef OBJ
	    gen(O_AS2, O_AS2, sizeof(long), width(inittype));
		/*
		 * compute and save the termination expression
		 */
	    put(2, O_LV | cbn<<8+INDX, termnlp -> value[ NL_OFFS ] );
#	endif OBJ
	termtype = rvalue( term , fortype , RREQ );
	if ( incompat( termtype , fortype , term ) ) {
	    cerror("Type of limit expression clashed with index type in 'for' statement");
	    if (forvar != NIL) {
		forvar->value[ NL_FORV ] = FORVAR;
	    }
	    statement( stat );
	    goto byebye;
	}
#	ifdef PC
	    sconv(p2type(termtype), P2INT);
	    putop( P2ASSIGN , P2INT );
	    putdot( filename , line );
		/*
		 * we can skip the loop altogether if !( init <= term )
		 */
	    after = getlab();
	    putRV( 0 , cbn , initnlp -> value[ NL_OFFS ] ,
		    initnlp -> extra_flags , P2INT );
	    putRV( 0 , cbn , termnlp -> value[ NL_OFFS ] ,
		    termnlp -> extra_flags , P2INT );
	    putop( ( arg[0] == T_FORU ? P2LE : P2GE ) , P2INT );
	    putleaf( P2ICON , after , 0 , P2INT , 0 );
	    putop( P2CBRANCH , P2INT );
	    putdot( filename , line );
		/*
		 * okay, so we have to execute the loop body,
		 * but first, if checking is on,
		 * check that the termination expression
		 * is assignment compatible with the control-variable.
		 */
	    if (opt('t')) {
		precheck(fortype, "_RANG4", "_RSNG4");
		putRV(0, cbn, termnlp -> value[NL_OFFS],
		    termnlp -> extra_flags, P2INT);
		postcheck(fortype, nl+T4INT);
		putdot(filename, line);
	    }
		/*
		 * assign the initial expression to the shadow
		 * checking the assignment if necessary.
		 */
	    putRV(0, cbn, shadownlp -> value[NL_OFFS],
		shadownlp -> extra_flags, forp2type);
	    if (opt('t')) {
		precheck(fortype, "_RANG4", "_RSNG4");
		putRV(0, cbn, initnlp -> value[NL_OFFS],
		    initnlp -> extra_flags, P2INT);
		postcheck(fortype, nl+T4INT);
	    } else {
		putRV(0, cbn, initnlp -> value[NL_OFFS],
		    initnlp -> extra_flags, P2INT);
	    }
	    sconv(P2INT, forp2type);
	    putop(P2ASSIGN, forp2type);
	    putdot(filename, line);
		/*
		 * put down the label at the top of the loop
		 */
	    again = getlab();
	    putlab( again );
		/*
		 * each time through the loop
		 * assign the shadow to the for variable.
		 */
	    lvalue(lhs, NOUSE, RREQ);
	    putRV(0, cbn, shadownlp -> value[NL_OFFS],
		    shadownlp -> extra_flags, forp2type);
	    putop(P2ASSIGN, forp2type);
	    putdot(filename, line);
#	endif PC
#	ifdef OBJ
	    gen(O_AS2, O_AS2, sizeof(long), width(termtype));
		/*
		 * we can skip the loop altogether if !( init <= term )
		 */
	    put(2, O_RV4 | cbn<<8+INDX, initnlp -> value[ NL_OFFS ] );
	    put(2, O_RV4 | cbn<<8+INDX, termnlp -> value[ NL_OFFS ] );
	    gen(NIL, arg[0] == T_FORU ? T_LE : T_GE, sizeof(long),
			sizeof(long));
	    after = getlab();
	    put(2, O_IF, after);
		/*
		 * okay, so we have to execute the loop body,
		 * but first, if checking is on,
		 * check that the termination expression
		 * is assignment compatible with the control-variable.
		 */
	    if (opt('t')) {
		put(2, O_LV | cbn<<8+INDX, shadownlp -> value[ NL_OFFS ] );
		put(2, O_RV4 | cbn<<8+INDX, termnlp -> value[ NL_OFFS ] );
		rangechk(fortype, nl+T4INT);
		gen(O_AS2, O_AS2, forwidth, sizeof(long));
	    }
		/*
		 * assign the initial expression to the shadow
		 * checking the assignment if necessary.
		 */
	    put(2, O_LV | cbn<<8+INDX, shadownlp -> value[ NL_OFFS ] );
	    put(2, O_RV4 | cbn<<8+INDX, initnlp -> value[ NL_OFFS ] );
	    rangechk(fortype, nl+T4INT);
	    gen(O_AS2, O_AS2, forwidth, sizeof(long));
		/*
		 * put down the label at the top of the loop
		 */
	    again = getlab();
	    putlab( again );
		/*
		 * each time through the loop
		 * assign the shadow to the for variable.
		 */
	    lvalue(lhs, NOUSE, RREQ);
	    stackRV(shadownlp);
	    gen(O_AS2, O_AS2, forwidth, sizeof(long));
#	endif OBJ
	    /*
	     *	shadowing the real for variable
	     *	with the shadow temporary:
	     *	save the real for variable flags (including nl_block).
	     *	replace them with the shadow's offset,
	     *	and mark the for variable as being a for variable.
	     */
	shadownlp -> nl_flags = forvar -> nl_flags;
	*forvar = *shadownlp;
	forvar -> symbol = saved_nl.symbol;
	forvar -> nl_next = saved_nl.nl_next;
	forvar -> type = saved_nl.type;
	forvar -> value[ NL_FORV ] = FORVAR;
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
		    putRV( STMTCOUNT , 0 , 0 , NGLOBAL , P2INT );
		    putleaf( P2ICON , 1 , 0 , P2INT , 0 );
		    putop( P2ASG P2PLUS , P2INT );
		    putdot( filename , line );
		}
	    }
	    /*rvalue( lhs , NIL , RREQ );*/
	    putRV( 0 , cbn , shadownlp -> value[ NL_OFFS ] ,
		    shadownlp -> extra_flags , forp2type );
	    sconv(forp2type, P2INT);
	    putRV( 0 , cbn , termnlp -> value[ NL_OFFS ] ,
		    termnlp -> extra_flags , P2INT );
	    putop( ( arg[ 0 ] == T_FORU ? P2LT : P2GT ) , P2INT );
	    putleaf( P2ICON , after , 0 , P2INT , 0 );
	    putop( P2CBRANCH , P2INT );
	    putdot( filename , line );
		/*
		 * okay, so we have to do it again,
		 * but first, increment the for variable.
		 * no need to rangecheck it, since we checked the
		 * termination value before we started.
		 */
	    /*lvalue( lhs , MOD , RREQ );*/
	    putRV( 0 , cbn , shadownlp -> value[ NL_OFFS ] ,
		    shadownlp -> extra_flags , forp2type );
	    /*rvalue( lhs , NIL , RREQ );*/
	    putRV( 0 , cbn , shadownlp -> value[ NL_OFFS ] ,
		    shadownlp -> extra_flags , forp2type );
	    sconv(forp2type, P2INT);
	    putleaf( P2ICON , 1 , 0 , P2INT , 0 );
	    putop( ( arg[0] == T_FORU ? P2PLUS : P2MINUS ) , P2INT );
	    sconv(P2INT, forp2type);
	    putop( P2ASSIGN , forp2type );
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
		 * it has been reached, else updating the index variable,
		 * and returning to the top of the loop.
		 */
	    putline();
	    put(2, O_RV4 | cbn<<8+INDX, termnlp -> value[ NL_OFFS ] );
	    put(2, O_LV | cbn<<8+INDX, shadownlp -> value[ NL_OFFS ] );
	    put(2, (arg[0] == T_FORU ? O_FOR1U : O_FOR1D) + (forwidth >> 1),
		    again);
		/*
		 * and here we are
		 */
	    patch( after );
#	endif OBJ
byebye:
	noreach = 0;
	if (forvar != NIL) {
	    saved_nl.nl_flags |= forvar -> nl_flags & (NUSED|NMOD);
	    *forvar = saved_nl;
	}
	if ( goc != gocnt ) {
	    putcnt();
	}
    }
