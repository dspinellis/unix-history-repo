/* Copyright (c) 1979 Regents of the University of California */

#ifndef lint
static	char sccsid[] = "@(#)call.c 1.26 %G%";
#endif

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
#   include "pc.h"
#   include "pcops.h"
#endif PC
#include "tmps.h"
#include "tree_ty.h"

/*
 * Call generates code for calls to
 * user defined procedures and functions
 * and is called by proc and funccod.
 * P is the result of the lookup
 * of the procedure/function symbol,
 * and porf is PROC or FUNC.
 * Psbn is the block number of p.
 *
 *	the idea here is that regular scalar functions are just called,
 *	while structure functions and formal functions have their results
 *	stored in a temporary after the call.
 *	structure functions do this because they return pointers
 *	to static results, so we copy the static
 *	and return a pointer to the copy.
 *	formal functions do this because we have to save the result
 *	around a call to the runtime routine which restores the display,
 *	so we can't just leave the result lying around in registers.
 *	formal calls save the address of the descriptor in a local
 *	temporary, so it can be addressed for the call which restores
 *	the display (FRTN).
 *	calls to formal parameters pass the formal as a hidden argument 
 *	to a special entry point for the formal call.
 *	[this is somewhat dependent on the way arguments are addressed.]
 *	so PROCs and scalar FUNCs look like
 *		p(...args...)
 *	structure FUNCs look like
 *		(temp = p(...args...),&temp)
 *	formal FPROCs look like
 *		( t=p,( t -> entryaddr )(...args...,t,s),FRTN(t,s))
 *	formal scalar FFUNCs look like
 *		( t=p,temp=( t -> entryaddr )(...args...,t,s),FRTN(t,s),temp)
 *	formal structure FFUNCs look like
 *		(t=p,temp = ( t -> entryaddr )(...args...,t,s),FRTN(t,s),&temp)
 */
struct nl *
call(p, argv_node, porf, psbn)
	struct nl *p;
	struct tnode	*argv_node;	/* list node */
	int porf, psbn;
{
	register struct nl *p1, *q;
	struct tnode *rnode;
	bool chk = TRUE;
 	struct nl	*savedispnp;	/* temporary to hold saved display */
#	ifdef PC
	    int		p_type_class = classify( p -> type );
	    long	p_type_p2type = p2type( p -> type );
	    bool	noarguments;
		/*
		 *	these get used if temporaries and structures are used
		 */
	    struct nl	*tempnlp;
	    long	temptype;	/* type of the temporary */
	    long	p_type_width;
	    long	p_type_align;
	    char	extname[ BUFSIZ ];
	    struct nl	*tempdescrp;
#	endif PC

         if (p->class == FFUNC || p->class == FPROC) {
 	    /*
 	     * allocate space to save the display for formal calls
 	     */
	    savedispnp = tmpalloc( (long) sizeof display , NLNIL , NOREG );
 	}
#	ifdef OBJ
	    if (p->class == FFUNC || p->class == FPROC) {
 		(void) put(2, O_LV | cbn << 8 + INDX ,
 			(int) savedispnp -> value[ NL_OFFS ] );
		(void) put(2, PTR_RV | psbn << 8+INDX, (int)p->value[NL_OFFS]);
	    }
	    if (porf == FUNC) {
		    /*
		     * Push some space
		     * for the function return type
		     */
		    (void) put(2, O_PUSH, leven(-lwidth(p->type)));
	    }
#	endif OBJ
#	ifdef PC
		/*
		 *	if this is a formal call,
		 *	stash the address of the descriptor
		 *	in a temporary so we can find it
		 *	after the FCALL for the call to FRTN
		 */
	    if ( p -> class == FFUNC || p -> class == FPROC ) {
		tempdescrp = tmpalloc((long) (sizeof( struct formalrtn *)),
					NLNIL, REGOK );
		putRV((char *) 0 , cbn , tempdescrp -> value[ NL_OFFS ] ,
			tempdescrp -> extra_flags , P2PTR|P2STRTY );
		putRV((char *) 0 , psbn , p -> value[ NL_OFFS ] ,
			p -> extra_flags , P2PTR|P2STRTY );
		putop( P2ASSIGN , P2PTR | P2STRTY );
	    }
		/*
		 *	if we have to store a temporary,
		 *	temptype will be its type,
		 *	otherwise, it's P2UNDEF.
		 */
	    temptype = P2UNDEF;
	    if ( porf == FUNC ) {
		p_type_width = width( p -> type );
		switch( p_type_class ) {
		    case TSTR:
		    case TSET:
		    case TREC:
		    case TFILE:
		    case TARY:
			temptype = P2STRTY;
			p_type_align = align( p -> type );
			break;
		    default:
			if ( p -> class == FFUNC ) {
			    temptype = p2type( p -> type );
			}
			break;
		}
		if ( temptype != P2UNDEF ) {
		    tempnlp = tmpalloc(p_type_width, p -> type, NOREG);
			/*
			 *	temp
			 *	for (temp = ...
			 */
		    putRV((char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
			    tempnlp -> extra_flags , (int) temptype );
		}
	    }
	    switch ( p -> class ) {
		case FUNC:
		case PROC:
			/*
			 *	... p( ...
			 */
		    sextname( extname , p -> symbol , BLOCKNO(p -> nl_block) );
		    putleaf( P2ICON , 0 , 0 , p2type( p ) , extname );
		    break;
		case FFUNC:
		case FPROC:

			    /*
			     *	... ( t -> entryaddr )( ...
			     */
			    /* 	the descriptor */
			putRV((char *) 0 , cbn , tempdescrp -> value[ NL_OFFS ] ,
				tempdescrp -> extra_flags , P2PTR | P2STRTY );
			    /*	the entry address within the descriptor */
			if ( FENTRYOFFSET != 0 ) {
			    putleaf( P2ICON , FENTRYOFFSET , 0 , P2INT , 
						(char *) 0 );
			    putop( P2PLUS , 
				ADDTYPE(
				    ADDTYPE( ADDTYPE( p2type( p ) , P2FTN ) ,
					    P2PTR ) ,
					P2PTR ) );
			}
			    /*
			     *	indirect to fetch the formal entry address
			     *	with the result type of the routine.
			     */
			if (p -> class == FFUNC) {
			    putop( P2UNARY P2MUL ,
				ADDTYPE(ADDTYPE(p2type(p -> type), P2FTN),
					P2PTR));
			} else {
				/* procedures are int returning functions */
			    putop( P2UNARY P2MUL ,
				ADDTYPE(ADDTYPE(P2INT, P2FTN), P2PTR));
			}
			break;
		default:
			panic("call class");
	    }
	    noarguments = TRUE;
#	endif PC
	/*
	 * Loop and process each of
	 * arguments to the proc/func.
	 *	... ( ... args ... ) ...
	 */
	for (p1 = plist(p); p1 != NLNIL; p1 = p1->chain) {
	    if (argv_node == TR_NIL) {
		    error("Not enough arguments to %s", p->symbol);
		    return (NLNIL);
	    }
	    switch (p1->class) {
		case REF:
			/*
			 * Var parameter
			 */
			rnode = argv_node->list_node.list;
			if (rnode != TR_NIL && rnode->tag != T_VAR) {
				error("Expression given (variable required) for var parameter %s of %s", p1->symbol, p->symbol);
				chk = FALSE;
				break;
			}
			q = lvalue( argv_node->list_node.list,
					MOD | ASGN , LREQ );
			if (q == NIL) {
				chk = FALSE;
				break;
			}
			if (q != p1->type) {
				error("Parameter type not identical to type of var parameter %s of %s", p1->symbol, p->symbol);
				chk = FALSE;
				break;
			}
			break;
		case VAR:
			/*
			 * Value parameter
			 */
#			ifdef OBJ
			    q = rvalue(argv_node->list_node.list,
					p1->type , RREQ );
#			endif OBJ
#			ifdef PC
				/*
				 * structure arguments require lvalues,
				 * scalars use rvalue.
				 */
			    switch( classify( p1 -> type ) ) {
				case TFILE:
				case TARY:
				case TREC:
				case TSET:
				case TSTR:
				q = stkrval(argv_node->list_node.list,
						p1 -> type , (long) LREQ );
				    break;
				case TINT:
				case TSCAL:
				case TBOOL:
				case TCHAR:
				    precheck( p1 -> type , "_RANG4" , "_RSNG4" );
				q = stkrval(argv_node->list_node.list,
						p1 -> type , (long) RREQ );
				    postcheck(p1 -> type, nl+T4INT);
				    break;
				case TDOUBLE:
				q = stkrval(argv_node->list_node.list,
						p1 -> type , (long) RREQ );
				    sconv(p2type(q), P2DOUBLE);
				    break;
				default:
				    q = rvalue(argv_node->list_node.list,
						p1 -> type , RREQ );
				    break;
			    }
#			endif PC
			if (q == NIL) {
				chk = FALSE;
				break;
			}
			if (incompat(q, p1->type,
				argv_node->list_node.list)) {
				cerror("Expression type clashed with type of value parameter %s of %s", p1->symbol, p->symbol);
				chk = FALSE;
				break;
			}
#			ifdef OBJ
			    if (isa(p1->type, "bcsi"))
				    rangechk(p1->type, q);
			    if (q->class != STR)
				    convert(q, p1->type);
#			endif OBJ
#			ifdef PC
			    switch( classify( p1 -> type ) ) {
				case TFILE:
				case TARY:
				case TREC:
				case TSET:
				case TSTR:
					putstrop( P2STARG
					    , p2type( p1 -> type )
					    , (int) lwidth( p1 -> type )
					    , align( p1 -> type ) );
			    }
#			endif PC
			break;
		case FFUNC:
			/*
			 * function parameter
			 */
			q = flvalue(argv_node->list_node.list, p1 );
			/*chk = (chk && fcompat(q, p1));*/
			if ((chk) && (fcompat(q, p1)))
			    chk = TRUE;
			else
			    chk = FALSE;
			break;
		case FPROC:
			/*
			 * procedure parameter
			 */
			q = flvalue(argv_node->list_node.list, p1 );
			/* chk = (chk && fcompat(q, p1)); */
			if ((chk) && (fcompat(q, p1)))
			    chk = TRUE;
			else chk = FALSE;
			break;
		default:
			panic("call");
	    }
#	    ifdef PC
		    /*
		     *	if this is the nth (>1) argument,
		     *	hang it on the left linear list of arguments
		     */
		if ( noarguments ) {
			noarguments = FALSE;
		} else {
			putop( P2LISTOP , P2INT );
		}
#	    endif PC
	    argv_node = argv_node->list_node.next;
	}
	if (argv_node != TR_NIL) {
		error("Too many arguments to %s", p->symbol);
		rvlist(argv_node);
		return (NLNIL);
	}
	if (chk == FALSE)
		return NLNIL;
#	ifdef OBJ
	    if ( p -> class == FFUNC || p -> class == FPROC ) {
		(void) put(2, PTR_RV | psbn << 8+INDX, (int)p->value[NL_OFFS]);
 		(void) put(2, O_LV | cbn << 8 + INDX ,
 			(int) savedispnp -> value[ NL_OFFS ] );
		(void) put(1, O_FCALL);
		(void) put(2, O_FRTN, even(width(p->type)));
	    } else {
		(void) put(2, O_CALL | psbn << 8, (long)p->value[NL_ENTLOC]);
	    }
#	endif OBJ
#	ifdef PC
		/*
		 *	for formal calls: add the hidden argument
		 *	which is the formal struct describing the
		 *	environment of the routine.
		 *	and the argument which is the address of the
		 *	space into which to save the display.
		 */
	    if ( p -> class == FFUNC || p -> class == FPROC ) {
		putRV((char *) 0 , cbn , tempdescrp -> value[ NL_OFFS ] ,
			tempdescrp -> extra_flags , P2PTR|P2STRTY );
		if ( !noarguments ) {
		    putop( P2LISTOP , P2INT );
		}
		noarguments = FALSE;
 		putLV((char *) 0 , cbn , savedispnp -> value[ NL_OFFS ] ,
 			savedispnp -> extra_flags , P2PTR | P2STRTY );
 		putop( P2LISTOP , P2INT );
	    }
		/*
		 *	do the actual call:
		 *	    either	... p( ... ) ...
		 *	    or		... ( t -> entryaddr )( ... ) ...
		 *	and maybe an assignment.
		 */
	    if ( porf == FUNC ) {
		switch ( p_type_class ) {
		    case TBOOL:
		    case TCHAR:
		    case TINT:
		    case TSCAL:
		    case TDOUBLE:
		    case TPTR:
			putop( ( noarguments ? P2UNARY P2CALL : P2CALL ) ,
				(int) p_type_p2type );
			if ( p -> class == FFUNC ) {
			    putop( P2ASSIGN , (int) p_type_p2type );
			}
			break;
		    default:
			putstrop( ( noarguments ? P2UNARY P2STCALL : P2STCALL ),
				(int) ADDTYPE( p_type_p2type , P2PTR ) ,
				(int) p_type_width ,(int) p_type_align );
			putstrop(P2STASG, (int) ADDTYPE(p_type_p2type, P2PTR),
				(int) lwidth(p -> type), align(p -> type));
			break;
		}
	    } else {
		putop( ( noarguments ? P2UNARY P2CALL : P2CALL ) , P2INT );
	    }
		/*
		 *	( t=p , ... , FRTN( t ) ...
		 */
	    if ( p -> class == FFUNC || p -> class == FPROC ) {
		putop( P2COMOP , P2INT );
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR ) ,
			"_FRTN" );
		putRV((char *) 0 , cbn , tempdescrp -> value[ NL_OFFS ] ,
			tempdescrp -> extra_flags , P2PTR | P2STRTY );
 		putLV((char *) 0 , cbn , savedispnp -> value[ NL_OFFS ] ,
 			savedispnp -> extra_flags , P2PTR | P2STRTY );
 		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putop( P2COMOP , P2INT );
	    }
		/*
		 *	if required:
		 *	either	... , temp )
		 *	or	... , &temp )
		 */
	    if ( porf == FUNC && temptype != P2UNDEF ) {
		if ( temptype != P2STRTY ) {
		    putRV((char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
			    tempnlp -> extra_flags , (int) p_type_p2type );
		} else {
		    putLV((char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
			    tempnlp -> extra_flags , (int) p_type_p2type );
		}
		putop( P2COMOP , P2INT );
	    }
	    if ( porf == PROC ) {
		putdot( filename , line );
	    }
#	endif PC
	return (p->type);
}

rvlist(al)
	register struct tnode *al;
{

	for (; al != TR_NIL; al = al->list_node.next)
		(void) rvalue( al->list_node.list, NLNIL , RREQ );
}

    /*
     *	check that two function/procedure namelist entries are compatible
     */
bool
fcompat( formal , actual )
    struct nl	*formal;
    struct nl	*actual;
{
    register struct nl	*f_chain;
    register struct nl	*a_chain;
    extern struct nl	*plist();
    bool compat = TRUE;

    if ( formal == NLNIL || actual == NLNIL ) {
	return FALSE;
    }
    for (a_chain = plist(actual), f_chain = plist(formal);
         f_chain != NLNIL;
	 f_chain = f_chain->chain, a_chain = a_chain->chain) {
	if (a_chain == NIL) {
	    error("%s %s declared on line %d has more arguments than",
		parnam(formal->class), formal->symbol,
		(char *) linenum(formal));
	    cerror("%s %s declared on line %d",
		parnam(actual->class), actual->symbol,
		(char *) linenum(actual));
	    return FALSE;
	}
	if ( a_chain -> class != f_chain -> class ) {
	    error("%s parameter %s of %s declared on line %d is not identical",
		parnam(f_chain->class), f_chain->symbol,
		formal->symbol, (char *) linenum(formal));
	    cerror("with %s parameter %s of %s declared on line %d",
		parnam(a_chain->class), a_chain->symbol,
		actual->symbol, (char *) linenum(actual));
	    compat = FALSE;
	} else if (a_chain->class == FFUNC || a_chain->class == FPROC) {
	    /*compat = (compat && fcompat(f_chain, a_chain));*/
	    if ((compat) && (fcompat(f_chain, a_chain)))
		compat = TRUE;
	    else compat = FALSE;
	}
	if ((a_chain->class != FPROC && f_chain->class != FPROC) &&
	    (a_chain->type != f_chain->type)) {
	    error("Type of %s parameter %s of %s declared on line %d is not identical",
		parnam(f_chain->class), f_chain->symbol,
		formal->symbol, (char *) linenum(formal));
	    cerror("to type of %s parameter %s of %s declared on line %d",
		parnam(a_chain->class), a_chain->symbol,
		actual->symbol, (char *) linenum(actual));
	    compat = FALSE;
	}
    }
    if (a_chain != NIL) {
	error("%s %s declared on line %d has fewer arguments than",
	    parnam(formal->class), formal->symbol,
	    (char *) linenum(formal));
	cerror("%s %s declared on line %d",
	    parnam(actual->class), actual->symbol,
	    (char *) linenum(actual));
	return FALSE;
    }
    return compat;
}

char *
parnam(nltype)
    int nltype;
{
    switch(nltype) {
	case REF:
	    return "var";
	case VAR:
	    return "value";
	case FUNC:
	case FFUNC:
	    return "function";
	case PROC:
	case FPROC:
	    return "procedure";
	default:
	    return "SNARK";
    }
}

struct nl *plist(p)
    struct nl *p;
{
    switch (p->class) {
	case FFUNC:
	case FPROC:
	    return p->ptr[ NL_FCHAIN ];
	case PROC:
	case FUNC:
	    return p->chain;
	default:
	    {
		panic("plist");
		return(NLNIL); /* this is here only so lint won't complain
				  panic actually aborts */
	    }

    }
}

linenum(p)
    struct nl *p;
{
    if (p->class == FUNC)
	return p->ptr[NL_FVAR]->value[NL_LINENO];
    return p->value[NL_LINENO];
}
