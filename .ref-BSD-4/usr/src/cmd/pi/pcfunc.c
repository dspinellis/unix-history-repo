/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)pcfunc.c 1.3 10/19/80";

#include "whoami.h"
#ifdef PC
    /*
     *	and to the end of the file
     */
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include	"pc.h"
#include	"pcops.h"

/*
 * Funccod generates code for
 * built in function calls and calls
 * call to generate calls to user
 * defined functions and procedures.
 */
pcfunccod( r )
	int	 *r;
{
	struct nl *p;
	register struct nl *p1;
	register int *al;
	register op;
	int argc, *argv;
	int tr[2], tr2[4];
	char		*funcname;
	long		tempoff;
	long		temptype;
	struct nl	*rettype;

	/*
	 * Verify that the given name
	 * is defined and the name of
	 * a function.
	 */
	p = lookup(r[2]);
	if (p == NIL) {
		rvlist(r[3]);
		return (NIL);
	}
	if (p->class != FUNC && p->class != FFUNC) {
		error("%s is not a function", p->symbol);
		rvlist(r[3]);
		return (NIL);
	}
	argv = r[3];
	/*
	 * Call handles user defined
	 * procedures and functions
	 */
	if (bn != 0)
		return (call(p, argv, FUNC, bn));
	/*
	 * Count the arguments
	 */
	argc = 0;
	for (al = argv; al != NIL; al = al[2])
		argc++;
	/*
	 * Built-in functions have
	 * their interpreter opcode
	 * associated with them.
	 */
	op = p->value[0] &~ NSTAND;
	if (opt('s') && (p->value[0] & NSTAND)) {
		standard();
		error("%s is a nonstandard function", p->symbol);
	}
	if ( op == O_ARGC ) {
	    putleaf( P2NAME , 0 , 0 , P2INT , "__argc" );
	    return nl + T4INT;
	}
	switch (op) {
		/*
		 * Parameterless functions
		 */
		case O_CLCK:
			funcname = "_CLCK";
			goto noargs;
		case O_SCLCK:
			funcname = "_SCLCK";
			goto noargs;
noargs:
			if (argc != 0) {
				error("%s takes no arguments", p->symbol);
				rvlist(argv);
				return (NIL);
			}
			putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR )
				, funcname );
			putop( P2UNARY P2CALL , P2INT );
			return (nl+T4INT);
		case O_WCLCK:
			if (argc != 0) {
				error("%s takes no arguments", p->symbol);
				rvlist(argv);
				return (NIL);
			}
			putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR )
				, "_time" );
			putleaf( P2ICON , 0 , 0 , P2INT , 0 );
			putop( P2CALL , P2INT );
			return (nl+T4INT);
		case O_EOF:
		case O_EOLN:
			if (argc == 0) {
				argv = tr;
				tr[1] = tr2;
				tr2[0] = T_VAR;
				tr2[2] = input->symbol;
				tr2[1] = tr2[3] = NIL;
				argc = 1;
			} else if (argc != 1) {
				error("%s takes either zero or one argument", p->symbol);
				rvlist(argv);
				return (NIL);
			}
		}
	/*
	 * All other functions take
	 * exactly one argument.
	 */
	if (argc != 1) {
		error("%s takes exactly one argument", p->symbol);
		rvlist(argv);
		return (NIL);
	}
	/*
	 * find out the type of the argument
	 */
	codeoff();
	p1 = stkrval((int *) argv[1], NLNIL , RREQ );
	codeon();
	if (p1 == NIL)
		return (NIL);
	/*
	 * figure out the return type and the funtion name
	 */
	switch (op) {
	    case O_EXP:
		    funcname = "_exp";
		    goto mathfunc;
	    case O_SIN:
		    funcname = "_sin";
		    goto mathfunc;
	    case O_COS:
		    funcname = "_cos";
		    goto mathfunc;
	    case O_ATAN:
		    funcname = "_atan";
		    goto mathfunc;
	    case O_LN:
		    funcname = opt('t') ? "_LN" : "_log";
		    goto mathfunc;
	    case O_SQRT:
		    funcname = opt('t') ? "_SQRT" : "_sqrt";
		    goto mathfunc;
	    case O_RANDOM:
		    funcname = "_RANDOM";
		    goto mathfunc;
mathfunc:
		    if (isnta(p1, "id")) {
			    error("%s's argument must be integer or real, not %s", p->symbol, nameof(p1));
			    return (NIL);
		    }
		    putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2DOUBLE , P2PTR ) , funcname );
		    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
		    if ( isa( p1 , "i" ) ) {
			putop( P2SCONV , P2DOUBLE );
		    }
		    putop( P2CALL , P2DOUBLE );
		    return nl + TDOUBLE;
	    case O_EXPO:
		    if (isnta( p1 , "id" ) ) {
			    error("%s's argument must be integer or real, not %s", p->symbol, nameof(p1));
			    return NIL;
		    }
		    putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_EXPO" );
		    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
		    if ( isa( p1 , "i" ) ) {
			putop( P2SCONV , P2DOUBLE );
		    }
		    putop( P2CALL , P2INT );
		    return ( nl + T4INT );
	    case O_UNDEF:
		    if ( isnta( p1 , "id" ) ) {
			    error("%s's argument must be integer or real, not %s", p->symbol, nameof(p1));
			    return NIL;
		    }
		    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
		    putleaf( P2ICON , 0 , 0 , P2INT , 0 );
		    putop( P2COMOP , P2INT );
		    return ( nl + TBOOL );
	    case O_SEED:
		    if (isnta(p1, "i")) {
			    error("seed's argument must be an integer, not %s", nameof(p1));
			    return (NIL);
		    }
		    putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_SEED" );
		    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
		    putop( P2CALL , P2INT );
		    return nl + T4INT;
	    case O_ROUND:
	    case O_TRUNC:
		    if ( isnta( p1 , "d" ) ) {
			    error("%s's argument must be a real, not %s", p->symbol, nameof(p1));
			    return (NIL);
		    }
		    putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR )
			    , op == O_ROUND ? "_ROUND" : "_TRUNC" );
		    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
		    putop( P2CALL , P2INT );
		    return nl + T4INT;
	    case O_ABS2:
			if ( isa( p1 , "d" ) ) {
			    putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2DOUBLE , P2PTR )
				, "_fabs" );
			    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
			    putop( P2CALL , P2DOUBLE );
			    return nl + TDOUBLE;
			}
			if ( isa( p1 , "i" ) ) {
			    putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR ) , "_abs" );
			    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
			    putop( P2CALL , P2INT );
			    return nl + T4INT;
			}
			error("%s's argument must be an integer or real, not %s", p->symbol, nameof(p1));
			return NIL;
	    case O_SQR2:
			if ( isa( p1 , "d" ) ) {
			    temptype = P2DOUBLE;
			    rettype = nl + TDOUBLE;
			    sizes[ cbn ].om_off -= sizeof( double );
			} else if ( isa( p1 , "i" ) ) {
			    temptype = P2INT;
			    rettype = nl + T4INT;
			    sizes[ cbn ].om_off -= sizeof( long );
			} else {
			    error("%s's argument must be an integer or real, not %s", p->symbol, nameof(p1));
			    return NIL;
			}
			tempoff = sizes[ cbn ].om_off;
			if ( tempoff < sizes[ cbn ].om_max ) {
			    sizes[ cbn ].om_max = tempoff;
			}
			putlbracket( ftnno , -tempoff );
			putRV( 0 , cbn , tempoff , temptype , 0 );
			p1 = rvalue( (int *) argv[1] , NLNIL , RREQ );
			putop( P2ASSIGN , temptype );
			putRV( 0 , cbn , tempoff , temptype , 0 );
			putRV( 0 , cbn , tempoff , temptype , 0 );
			putop( P2MUL , temptype );
			putop( P2COMOP , temptype );
			return rettype;
	    case O_ORD2:
			p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
			if (isa(p1, "bcis") || classify(p1) == TPTR) {
				return (nl+T4INT);
			}
			error("ord's argument must be of scalar type or a pointer, not %s", nameof(p1));
			return (NIL);
	    case O_SUCC2:
	    case O_PRED2:
			if (isa(p1, "d")) {
				error("%s is forbidden for reals", p->symbol);
				return (NIL);
			}
			if ( isnta( p1 , "bcsi" ) ) {
			    error("%s's argument must be of scalar type, not %s", p->symbol, nameof(p1));
			    return NIL;
			}
			if ( opt( 't' ) ) {
			    putleaf( P2ICON , 0 , 0
				    , ADDTYPE( P2FTN | P2INT , P2PTR )
				    , op == O_SUCC2 ? "_SUCC" : "_PRED" );
			    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
			    putleaf( P2ICON , p1 -> range[0] , 0 , P2INT , 0 );
			    putop( P2LISTOP , P2INT );
			    putleaf( P2ICON , p1 -> range[1] , 0 , P2INT , 0 );
			    putop( P2LISTOP , P2INT );
			    putop( P2CALL , P2INT );
			} else {
			    p1 = rvalue( argv[1] , NIL , RREQ );
			    putleaf( P2ICON , 1 , 0 , P2INT , 0 );
			    putop( op == O_SUCC2 ? P2PLUS : P2MINUS , P2INT );
			}
			if ( isa( p1 , "bcs" ) ) {
			    return p1;
			} else {
			    return nl + T4INT;
			}
	    case O_ODD2:
			if (isnta(p1, "i")) {
				error("odd's argument must be an integer, not %s", nameof(p1));
				return (NIL);
			}
			p1 = rvalue( (int *) argv[1] , NLNIL , RREQ );
			putleaf( P2ICON , 1 , 0 , P2INT , 0 );
			putop( P2AND , P2INT );
			return nl + TBOOL;
	    case O_CHR2:
			if (isnta(p1, "i")) {
				error("chr's argument must be an integer, not %s", nameof(p1));
				return (NIL);
			}
			if (opt('t')) {
			    putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2CHAR , P2PTR ) , "_CHR" );
			    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
			    putop( P2CALL , P2CHAR );
			} else {
			    p1 = stkrval( (int *) argv[1] , NLNIL , RREQ );
			}
			return nl + TCHAR;
	    case O_CARD:
			if (isnta(p1, "t")) {
			    error("Argument to card must be a set, not %s", nameof(p1));
			    return (NIL);
			}
			putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_CARD" );
			p1 = stkrval( (int *) argv[1] , NLNIL , LREQ );
			putleaf( P2ICON , lwidth( p1 ) , 0 , P2INT , 0 );
			putop( P2LISTOP , P2INT );
			putop( P2CALL , P2INT );
			return nl + T2INT;
	    case O_EOLN:
			if (!text(p1)) {
				error("Argument to eoln must be a text file, not %s", nameof(p1));
				return (NIL);
			}
			putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_TEOLN" );
			p1 = stklval( (int *) argv[1] , NOFLAGS );
			putop( P2CALL , P2INT );
			return nl + TBOOL;
	    case O_EOF:
			if (p1->class != FILET) {
				error("Argument to eof must be file, not %s", nameof(p1));
				return (NIL);
			}
			putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_TEOF" );
			p1 = stklval( (int *) argv[1] , NOFLAGS );
			putop( P2CALL , P2INT );
			return nl + TBOOL;
	    case 0:
			error("%s is an unimplemented 6000-3.4 extension", p->symbol);
	    default:
			panic("func1");
	}
}
#endif PC
