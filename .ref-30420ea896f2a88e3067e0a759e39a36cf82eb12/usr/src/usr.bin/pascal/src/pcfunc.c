/* Copyright (c) 1979 Regents of the University of California */

#ifndef lint
static	char sccsid[] = "@(#)pcfunc.c 1.15 %G%";
#endif

#include "whoami.h"
#ifdef PC
    /*
     *	and to the end of the file
     */
#include "0.h"
#include "tree.h"
#include "objfmt.h"
#include "opcode.h"
#include "pc.h"
#include "pcops.h"
#include "tmps.h"
#include "tree_ty.h"

/*
 * Funccod generates code for
 * built in function calls and calls
 * call to generate calls to user
 * defined functions and procedures.
 */
struct nl *
pcfunccod( r )
	struct tnode	 *r; /* T_FCALL */
{
	struct nl *p;
	register struct nl *p1;
	register struct tnode *al;
	register op;
	int argc;
	struct tnode *argv;
	struct tnode tr, tr2;
	char		*funcname;
	struct nl	*tempnlp;
	long		temptype;
	struct nl	*rettype;

	/*
	 * Verify that the given name
	 * is defined and the name of
	 * a function.
	 */
	p = lookup(r->pcall_node.proc_id);
	if (p == NLNIL) {
		rvlist(r->pcall_node.arg);
		return (NLNIL);
	}
	if (p->class != FUNC && p->class != FFUNC) {
		error("%s is not a function", p->symbol);
		rvlist(r->pcall_node.arg);
		return (NLNIL);
	}
	argv = r->pcall_node.arg;
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
	for (al = argv; al != TR_NIL; al = al->list_node.next)
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
				return (NLNIL);
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
				return (NLNIL);
			}
			putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR )
				, "_time" );
			putleaf( P2ICON , 0 , 0 , P2INT , (char *) 0 );
			putop( P2CALL , P2INT );
			return (nl+T4INT);
		case O_EOF:
		case O_EOLN:
			if (argc == 0) {
				argv = &(tr);
				tr.list_node.list = &(tr2);
				tr2.tag = T_VAR;
				tr2.var_node.cptr = input->symbol;
				tr2.var_node.line_no = NIL;
				tr2.var_node.qual = TR_NIL;
				argc = 1;
			} else if (argc != 1) {
				error("%s takes either zero or one argument", p->symbol);
				rvlist(argv);
				return (NLNIL);
			}
		}
	/*
	 * All other functions take
	 * exactly one argument.
	 */
	if (argc != 1) {
		error("%s takes exactly one argument", p->symbol);
		rvlist(argv);
		return (NLNIL);
	}
	/*
	 * find out the type of the argument
	 */
	codeoff();
	p1 = stkrval( argv->list_node.list, NLNIL , (long) RREQ );
	codeon();
	if (p1 == NLNIL)
		return (NLNIL);
	/*
	 * figure out the return type and the funtion name
	 */
	switch (op) {
	    case 0:
			error("%s is an unimplemented 6000-3.4 extension", p->symbol);
	    default:
			panic("func1");
	    case O_EXP:
		    funcname = opt('t') ? "_EXP" : "_exp";
		    goto mathfunc;
	    case O_SIN:
		    funcname = opt('t') ? "_SIN" : "_sin";
		    goto mathfunc;
	    case O_COS:
		    funcname = opt('t') ? "_COS" : "_cos";
		    goto mathfunc;
	    case O_ATAN:
		    funcname = opt('t') ? "_ATAN" : "_atan";
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
			    return (NLNIL);
		    }
		    putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2DOUBLE , P2PTR ) , funcname );
		    p1 = stkrval(  argv->list_node.list , NLNIL , (long) RREQ );
		    sconv(p2type(p1), P2DOUBLE);
		    putop( P2CALL , P2DOUBLE );
		    return nl + TDOUBLE;
	    case O_EXPO:
		    if (isnta( p1 , "id" ) ) {
			    error("%s's argument must be integer or real, not %s", p->symbol, nameof(p1));
			    return NIL;
		    }
		    putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_EXPO" );
		    p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
		    sconv(p2type(p1), P2DOUBLE);
		    putop( P2CALL , P2INT );
		    return ( nl + T4INT );
	    case O_UNDEF:
		    if ( isnta( p1 , "id" ) ) {
			    error("%s's argument must be integer or real, not %s", p->symbol, nameof(p1));
			    return NLNIL;
		    }
		    p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
		    putleaf( P2ICON , 0 , 0 , P2CHAR , (char *) 0 );
		    putop( P2COMOP , P2CHAR );
		    return ( nl + TBOOL );
	    case O_SEED:
		    if (isnta(p1, "i")) {
			    error("seed's argument must be an integer, not %s", nameof(p1));
			    return (NLNIL);
		    }
		    putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_SEED" );
		    p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
		    putop( P2CALL , P2INT );
		    return nl + T4INT;
	    case O_ROUND:
	    case O_TRUNC:
		    if ( isnta( p1 , "d" ) ) {
			    error("%s's argument must be a real, not %s", p->symbol, nameof(p1));
			    return (NLNIL);
		    }
		    putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR )
			    , op == O_ROUND ? "_ROUND" : "_TRUNC" );
		    p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
		    putop( P2CALL , P2INT );
		    return nl + T4INT;
	    case O_ABS2:
			if ( isa( p1 , "d" ) ) {
			    putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2DOUBLE , P2PTR )
				, "_fabs" );
			    p1 = stkrval( argv->list_node.list , NLNIL ,(long) RREQ );
			    putop( P2CALL , P2DOUBLE );
			    return nl + TDOUBLE;
			}
			if ( isa( p1 , "i" ) ) {
			    putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR ) , "_abs" );
			    p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
			    putop( P2CALL , P2INT );
			    return nl + T4INT;
			}
			error("%s's argument must be an integer or real, not %s", p->symbol, nameof(p1));
			return NLNIL;
	    case O_SQR2:
			if ( isa( p1 , "d" ) ) {
			    temptype = P2DOUBLE;
			    rettype = nl + TDOUBLE;
			    tempnlp = tmpalloc((long) (sizeof(double)), rettype, REGOK);
			} else if ( isa( p1 , "i" ) ) {
			    temptype = P2INT;
			    rettype = nl + T4INT;
			    tempnlp = tmpalloc((long) (sizeof(long)), rettype, REGOK);
			} else {
			    error("%s's argument must be an integer or real, not %s", p->symbol, nameof(p1));
			    return NLNIL;
			}
			putRV( (char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
				tempnlp -> extra_flags , (char) temptype  );
			p1 = rvalue( argv->list_node.list , NLNIL , RREQ );
			sconv(p2type(p1), (int) temptype);
			putop( P2ASSIGN , (int) temptype );
			putRV((char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
				tempnlp -> extra_flags , (char) temptype );
			putRV((char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
				tempnlp -> extra_flags , (char) temptype );
			putop( P2MUL , (int) temptype );
			putop( P2COMOP , (int) temptype );
			return rettype;
	    case O_ORD2:
			p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
			if (isa(p1, "bcis")) {
				return (nl+T4INT);
			}
			if (classify(p1) == TPTR) {
			    if (!opt('s')) {
				return (nl+T4INT);
			    }
			    standard();
			}
			error("ord's argument must be of scalar type, not %s",
				nameof(p1));
			return (NLNIL);
	    case O_SUCC2:
	    case O_PRED2:
			if (isa(p1, "d")) {
				error("%s is forbidden for reals", p->symbol);
				return (NLNIL);
			}
			if ( isnta( p1 , "bcsi" ) ) {
			    error("%s's argument must be of scalar type, not %s", p->symbol, nameof(p1));
			    return NLNIL;
			}
			if ( opt( 't' ) ) {
			    putleaf( P2ICON , 0 , 0
				    , ADDTYPE( P2FTN | P2INT , P2PTR )
				    , op == O_SUCC2 ? "_SUCC" : "_PRED" );
			    p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
			    tempnlp = p1 -> class == TYPE ? p1 -> type : p1;
			    putleaf( P2ICON, (int) tempnlp -> range[0], 0, P2INT, (char *) 0 );
			    putop( P2LISTOP , P2INT );
			    putleaf( P2ICON, (int) tempnlp -> range[1], 0, P2INT, (char *) 0 );
			    putop( P2LISTOP , P2INT );
			    putop( P2CALL , P2INT );
			    sconv(P2INT, p2type(p1));
			} else {
			    p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
			    putleaf( P2ICON , 1 , 0 , P2INT , (char *) 0 );
			    putop( op == O_SUCC2 ? P2PLUS : P2MINUS , P2INT );
			    sconv(P2INT, p2type(p1));
			}
			if ( isa( p1 , "bcs" ) ) {
			    return p1;
			} else {
			    return nl + T4INT;
			}
	    case O_ODD2:
			if (isnta(p1, "i")) {
				error("odd's argument must be an integer, not %s", nameof(p1));
				return (NLNIL);
			}
			p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
			    /*
			     *	THIS IS MACHINE-DEPENDENT!!!
			     */
			putleaf( P2ICON , 1 , 0 , P2INT , (char *) 0 );
			putop( P2AND , P2INT );
			sconv(P2INT, P2CHAR);
			return nl + TBOOL;
	    case O_CHR2:
			if (isnta(p1, "i")) {
				error("chr's argument must be an integer, not %s", nameof(p1));
				return (NLNIL);
			}
			if (opt('t')) {
			    putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2CHAR , P2PTR ) , "_CHR" );
			    p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
			    putop( P2CALL , P2CHAR );
			} else {
			    p1 = stkrval( argv->list_node.list , NLNIL , (long) RREQ );
			    sconv(P2INT, P2CHAR);
			}
			return nl + TCHAR;
	    case O_CARD:
			if (isnta(p1, "t")) {
			    error("Argument to card must be a set, not %s", nameof(p1));
			    return (NLNIL);
			}
			putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_CARD" );
			p1 = stkrval( argv->list_node.list , NLNIL , (long) LREQ );
			putleaf( P2ICON , (int) lwidth( p1 ) , 0 , P2INT , (char *) 0 );
			putop( P2LISTOP , P2INT );
			putop( P2CALL , P2INT );
			return nl + T4INT;
	    case O_EOLN:
			if (!text(p1)) {
				error("Argument to eoln must be a text file, not %s", nameof(p1));
				return (NLNIL);
			}
			putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_TEOLN" );
			p1 = stklval( argv->list_node.list , NOFLAGS );
			putop( P2CALL , P2INT );
			sconv(P2INT, P2CHAR);
			return nl + TBOOL;
	    case O_EOF:
			if (p1->class != FILET) {
				error("Argument to eof must be file, not %s", nameof(p1));
				return (NLNIL);
			}
			putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_TEOF" );
			p1 = stklval( argv->list_node.list , NOFLAGS );
			putop( P2CALL , P2INT );
			sconv(P2INT, P2CHAR);
			return nl + TBOOL;
	}
}
#endif PC
