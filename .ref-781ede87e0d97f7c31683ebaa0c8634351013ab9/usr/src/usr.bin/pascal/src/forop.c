/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)forop.c	5.1 (Berkeley) %G%";
#endif not lint


#include	"whoami.h"
#include	"0.h"
#include	"opcode.h"
#include	"tree.h"
#include	"objfmt.h"
#ifdef PC
#    include	"pc.h"
#    include	<pcc.h>
#endif PC
#include	"tmps.h"
#include	"tree_ty.h"

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
forop( tree_node)
    struct tnode	*tree_node;
    {
	struct tnode	*lhs;
	VAR_NODE	*lhs_node;
	FOR_NODE	*f_node;
	struct nl	*forvar;
	struct nl	*fortype;
#ifdef PC
	int		forp2type;
#endif PC
	int		forwidth;
	struct tnode	*init_node;
	struct nl	*inittype;
	struct nl	*initnlp;	/* initial value namelist entry */
	struct tnode	*term_node;
	struct nl	*termtype;
	struct nl	*termnlp;	/* termination value namelist entry */
	struct nl	*shadownlp;	/* namelist entry for the shadow */
	struct tnode	*stat_node;
	int		goc;		/* saved gocnt */
	int		again;		/* label at the top of the loop */
	int		after;		/* label after the end of the loop */
	struct nl	saved_nl;	/* saved namelist entry for loop var */

	goc = gocnt;
	forvar = NLNIL;
	if ( tree_node == TR_NIL ) { 
	    goto byebye;
	}
	f_node = &(tree_node->for_node);
	if ( f_node->init_asg == TR_NIL ) {
	    goto byebye;
	}
	line = f_node->line_no;
	putline();
	lhs = f_node->init_asg->asg_node.lhs_var;
	init_node = f_node->init_asg->asg_node.rhs_expr;
	term_node = f_node->term_expr;
	stat_node = f_node->for_stmnt;
	if (lhs == TR_NIL) {
nogood:
	    if (forvar != NIL) {
		forvar->value[ NL_FORV ] = FORVAR;
	    }
	    (void) rvalue( init_node , NLNIL , RREQ ); 
	    (void) rvalue( term_node , NLNIL , RREQ );
	    statement( stat_node );
	    goto byebye;
	}
	else lhs_node = &(lhs->var_node);
	    /*
	     * and this marks the variable as used!!!
	     */
	forvar = lookup( lhs_node->cptr );
	if ( forvar == NIL ) {
	    goto nogood;
	}
	saved_nl = *forvar;
	if ( lhs_node->qual != TR_NIL ) {
	    error("For variable %s must be unqualified", forvar->symbol);
	    goto nogood;
	}
	if (forvar->class == WITHPTR) {
	    error("For variable %s cannot be an element of a record", 
			lhs_node->cptr);
	    goto nogood;
	}
	if ( opt('s') &&
	    ( ( bn != cbn ) ||
#ifdef OBJ
		(whereis(forvar->value[NL_OFFS], 0) == PARAMVAR)
#endif OBJ
#ifdef PC
		(whereis(forvar->value[NL_OFFS], forvar->extra_flags)
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
	if ( fortype == NLNIL ) {
	    goto nogood;
	}
	if ( isnta( fortype , "bcis" ) ) {
	    error("For variable %s cannot be %ss", forvar->symbol, nameof( fortype ) );
	    goto nogood;
	}
	if ( forvar->value[ NL_FORV ] & FORVAR ) {
	    error("Can't modify the for variable %s in the range of the loop", forvar->symbol);
	    forvar = NLNIL;
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
	initnlp = tmpalloc((long) sizeof(long), nl+T4INT, NOREG);
	termnlp = tmpalloc((long) sizeof(long), nl+T4INT, NOREG);
	shadownlp = tmpalloc((long) forwidth, fortype, REGOK);
#	ifdef PC
		/*
		 * compute and save the initial expression
		 */
	    putRV((char *) 0 , cbn , initnlp -> value[ NL_OFFS ] ,
		    initnlp -> extra_flags , PCCT_INT );
#	endif PC
#	ifdef OBJ
	    (void) put(2, O_LV | cbn<<8+INDX, initnlp -> value[ NL_OFFS ] );
#	endif OBJ
	inittype = rvalue( init_node , fortype , RREQ );
	if ( incompat( inittype , fortype , init_node ) ) {
	    cerror("Type of initial expression clashed with index type in 'for' statement");
	    if (forvar != NLNIL) {
		forvar->value[ NL_FORV ] = FORVAR;
	    }
	    (void) rvalue( term_node , NLNIL , RREQ );
	    statement( stat_node );
	    goto byebye;
	}
#	ifdef PC
	    sconv(p2type(inittype), PCCT_INT);
	    putop( PCC_ASSIGN , PCCT_INT );
	    putdot( filename , line );
		/*
		 * compute and save the termination expression
		 */
	    putRV((char *) 0 , cbn , termnlp -> value[ NL_OFFS ] ,
		    termnlp -> extra_flags , PCCT_INT );
#	endif PC
#	ifdef OBJ
	    (void) gen(O_AS2, O_AS2, sizeof(long), width(inittype));
		/*
		 * compute and save the termination expression
		 */
	    (void) put(2, O_LV | cbn<<8+INDX, termnlp -> value[ NL_OFFS ] );
#	endif OBJ
	termtype = rvalue( term_node , fortype , RREQ );
	if ( incompat( termtype , fortype , term_node ) ) {
	    cerror("Type of limit expression clashed with index type in 'for' statement");
	    if (forvar != NLNIL) {
		forvar->value[ NL_FORV ] = FORVAR;
	    }
	    statement( stat_node );
	    goto byebye;
	}
#	ifdef PC
	    sconv(p2type(termtype), PCCT_INT);
	    putop( PCC_ASSIGN , PCCT_INT );
	    putdot( filename , line );
		/*
		 * we can skip the loop altogether if !( init <= term )
		 */
	    after = (int) getlab();
	    putRV((char *) 0 , cbn , initnlp -> value[ NL_OFFS ] ,
		    initnlp -> extra_flags , PCCT_INT );
	    putRV((char *) 0 , cbn , termnlp -> value[ NL_OFFS ] ,
		    termnlp -> extra_flags , PCCT_INT );
	    putop( ( tree_node->tag == T_FORU ? PCC_LE : PCC_GE ) , PCCT_INT );
	    putleaf( PCC_ICON , after , 0 , PCCT_INT, (char *) 0 );
	    putop( PCC_CBRANCH , PCCT_INT );
	    putdot( filename , line );
		/*
		 * okay, so we have to execute the loop body,
		 * but first, if checking is on,
		 * check that the termination expression
		 * is assignment compatible with the control-variable.
		 */
	    if (opt('t')) {
		precheck(fortype, "_RANG4", "_RSNG4");
		putRV((char *) 0, cbn, termnlp -> value[NL_OFFS],
		    termnlp -> extra_flags, PCCT_INT);
		postcheck(fortype, nl+T4INT);
		putdot(filename, line);
	    }
		/*
		 * assign the initial expression to the shadow
		 * checking the assignment if necessary.
		 */
	    putRV((char *) 0, cbn, shadownlp -> value[NL_OFFS],
		shadownlp -> extra_flags, forp2type);
	    if (opt('t')) {
		precheck(fortype, "_RANG4", "_RSNG4");
		putRV((char *) 0, cbn, initnlp -> value[NL_OFFS],
		    initnlp -> extra_flags, PCCT_INT);
		postcheck(fortype, nl+T4INT);
	    } else {
		putRV((char *) 0, cbn, initnlp -> value[NL_OFFS],
		    initnlp -> extra_flags, PCCT_INT);
	    }
	    sconv(PCCT_INT, forp2type);
	    putop(PCC_ASSIGN, forp2type);
	    putdot(filename, line);
		/*
		 * put down the label at the top of the loop
		 */
	    again = (int) getlab();
	    (void) putlab((char *) again );
		/*
		 * each time through the loop
		 * assign the shadow to the for variable.
		 */
	    (void) lvalue(lhs, NOUSE, RREQ);
	    putRV((char *) 0, cbn, shadownlp -> value[NL_OFFS],
		    shadownlp -> extra_flags, forp2type);
	    putop(PCC_ASSIGN, forp2type);
	    putdot(filename, line);
#	endif PC
#	ifdef OBJ
	    (void) gen(O_AS2, O_AS2, sizeof(long), width(termtype));
		/*
		 * we can skip the loop altogether if !( init <= term )
		 */
	    (void) put(2, O_RV4 | cbn<<8+INDX, initnlp -> value[ NL_OFFS ] );
	    (void) put(2, O_RV4 | cbn<<8+INDX, termnlp -> value[ NL_OFFS ] );
	    (void) gen(NIL, tree_node->tag == T_FORU ? T_LE : T_GE, sizeof(long),
			sizeof(long));
	    after = (int) getlab();
	    (void) put(2, O_IF, after);
		/*
		 * okay, so we have to execute the loop body,
		 * but first, if checking is on,
		 * check that the termination expression
		 * is assignment compatible with the control-variable.
		 */
	    if (opt('t')) {
		(void) put(2, O_LV | cbn<<8+INDX, shadownlp -> value[ NL_OFFS ] );
		(void) put(2, O_RV4 | cbn<<8+INDX, termnlp -> value[ NL_OFFS ] );
		rangechk(fortype, nl+T4INT);
		(void) gen(O_AS2, O_AS2, forwidth, sizeof(long));
	    }
		/*
		 * assign the initial expression to the shadow
		 * checking the assignment if necessary.
		 */
	    (void) put(2, O_LV | cbn<<8+INDX, shadownlp -> value[ NL_OFFS ] );
	    (void) put(2, O_RV4 | cbn<<8+INDX, initnlp -> value[ NL_OFFS ] );
	    rangechk(fortype, nl+T4INT);
	    (void) gen(O_AS2, O_AS2, forwidth, sizeof(long));
		/*
		 * put down the label at the top of the loop
		 */
	    again = (int) getlab();
	    (void) putlab( (char *) again );
		/*
		 * each time through the loop
		 * assign the shadow to the for variable.
		 */
	    (void) lvalue(lhs, NOUSE, RREQ);
	    (void) stackRV(shadownlp);
	    (void) gen(O_AS2, O_AS2, forwidth, sizeof(long));
#	endif OBJ
	    /*
	     *	shadowing the real for variable
	     *	with the shadow temporary:
	     *	save the real for variable flags (including nl_block).
	     *	replace them with the shadow's offset,
	     *	and mark the for variable as being a for variable.
	     */
	shadownlp -> nl_flags |= NLFLAGS(forvar -> nl_flags);
	*forvar = *shadownlp;
	forvar -> symbol = saved_nl.symbol;
	forvar -> nl_next = saved_nl.nl_next;
	forvar -> type = saved_nl.type;
	forvar -> value[ NL_FORV ] = FORVAR;
	    /*
	     * and don't forget ...
	     */
	putcnt();
	statement( stat_node );
	    /*
	     * wasn't that fun?  do we get to do it again?
	     *	we don't do it again if ( !( forvar < limit ) )
	     *	pretend we were doing this at the top of the loop
	     */
	line = f_node->line_no;
#	ifdef PC
	    if ( opt( 'p' ) ) {
		if ( opt('t') ) {
		    putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			    , "_LINO" );
		    putop( PCCOM_UNARY PCC_CALL , PCCT_INT );
		    putdot( filename , line );
		} else {
		    putRV( STMTCOUNT , 0 , 0 , NGLOBAL , PCCT_INT );
		    putleaf( PCC_ICON , 1 , 0 , PCCT_INT , (char *) 0 );
		    putop( PCCOM_ASG PCC_PLUS , PCCT_INT );
		    putdot( filename , line );
		}
	    }
	    /*rvalue( lhs_node , NIL , RREQ );*/
	    putRV( (char *) 0 , cbn , shadownlp -> value[ NL_OFFS ] ,
		    shadownlp -> extra_flags , forp2type );
	    sconv(forp2type, PCCT_INT);
	    putRV( (char *) 0 , cbn , termnlp -> value[ NL_OFFS ] ,
		    termnlp -> extra_flags , PCCT_INT );
	    putop( ( tree_node->tag == T_FORU ? PCC_LT : PCC_GT ) , PCCT_INT );
	    putleaf( PCC_ICON , after , 0 , PCCT_INT , (char *) 0 );
	    putop( PCC_CBRANCH , PCCT_INT );
	    putdot( filename , line );
		/*
		 * okay, so we have to do it again,
		 * but first, increment the for variable.
		 * no need to rangecheck it, since we checked the
		 * termination value before we started.
		 */
	    /*lvalue( lhs , MOD , RREQ );*/
	    putRV( (char *) 0 , cbn , shadownlp -> value[ NL_OFFS ] ,
		    shadownlp -> extra_flags , forp2type );
	    /*rvalue( lhs_node , NIL , RREQ );*/
	    putRV( (char *) 0 , cbn , shadownlp -> value[ NL_OFFS ] ,
		    shadownlp -> extra_flags , forp2type );
	    sconv(forp2type, PCCT_INT);
	    putleaf( PCC_ICON , 1 , 0 , PCCT_INT , (char *) 0 );
	    putop( ( tree_node->tag == T_FORU ? PCC_PLUS : PCC_MINUS ) , PCCT_INT );
	    sconv(PCCT_INT, forp2type);
	    putop( PCC_ASSIGN , forp2type );
	    putdot( filename , line );
		/*
		 * and do it all again
		 */
	    putjbr( (long) again );
		/*
		 * and here we are
		 */
	    (void) putlab( (char *) after );
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
	    (void) put(2, O_RV4 | cbn<<8+INDX, termnlp -> value[ NL_OFFS ] );
	    (void) put(2, O_LV | cbn<<8+INDX, shadownlp -> value[ NL_OFFS ] );
	    (void) put(2, (tree_node->tag == T_FORU ? O_FOR1U : O_FOR1D) + (forwidth >> 1),
		    again);
		/*
		 * and here we are
		 */
	    patch( (PTR_DCL) after );
#	endif OBJ
byebye:
	noreach = FALSE;
	if (forvar != NLNIL) {
	    saved_nl.nl_flags |= NLFLAGS(forvar -> nl_flags) & (NUSED|NMOD);
	    *forvar = saved_nl;
	}
	if ( goc != gocnt ) {
	    putcnt();
	}
    }
