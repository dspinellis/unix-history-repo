/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)call.c	5.3 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "align.h"
#ifdef PC
#   include "pc.h"
#   include <pcc.h>
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
	register struct nl *p1, *q, *p2;
	register struct nl *ptype, *ctype;
	struct tnode *rnode;
	int i, j, d;
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
		    (void) put(2, O_PUSH,
			-roundup(lwidth(p->type), (long) A_STACK));
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
			tempdescrp -> extra_flags , PCCTM_PTR|PCCT_STRTY );
		putRV((char *) 0 , psbn , p -> value[ NL_OFFS ] ,
			p -> extra_flags , PCCTM_PTR|PCCT_STRTY );
		putop( PCC_ASSIGN , PCCTM_PTR | PCCT_STRTY );
	    }
		/*
		 *	if we have to store a temporary,
		 *	temptype will be its type,
		 *	otherwise, it's PCCT_UNDEF.
		 */
	    temptype = PCCT_UNDEF;
	    if ( porf == FUNC ) {
		p_type_width = width( p -> type );
		switch( p_type_class ) {
		    case TSTR:
		    case TSET:
		    case TREC:
		    case TFILE:
		    case TARY:
			temptype = PCCT_STRTY;
			p_type_align = align( p -> type );
			break;
		    default:
			if ( p -> class == FFUNC ) {
			    temptype = p2type( p -> type );
			}
			break;
		}
		if ( temptype != PCCT_UNDEF ) {
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
		    putleaf( PCC_ICON , 0 , 0 , p2type( p ) , extname );
		    break;
		case FFUNC:
		case FPROC:

			    /*
			     *	... ( t -> entryaddr )( ...
			     */
			    /* 	the descriptor */
			putRV((char *) 0 , cbn , tempdescrp -> value[ NL_OFFS ] ,
				tempdescrp -> extra_flags , PCCTM_PTR | PCCT_STRTY );
			    /*	the entry address within the descriptor */
			if ( FENTRYOFFSET != 0 ) {
			    putleaf( PCC_ICON , FENTRYOFFSET , 0 , PCCT_INT , 
						(char *) 0 );
			    putop( PCC_PLUS , 
				PCCM_ADDTYPE(
				    PCCM_ADDTYPE( PCCM_ADDTYPE( p2type( p ) , PCCTM_FTN ) ,
					    PCCTM_PTR ) ,
					PCCTM_PTR ) );
			}
			    /*
			     *	indirect to fetch the formal entry address
			     *	with the result type of the routine.
			     */
			if (p -> class == FFUNC) {
			    putop( PCCOM_UNARY PCC_MUL ,
				PCCM_ADDTYPE(PCCM_ADDTYPE(p2type(p -> type), PCCTM_FTN),
					PCCTM_PTR));
			} else {
				/* procedures are int returning functions */
			    putop( PCCOM_UNARY PCC_MUL ,
				PCCM_ADDTYPE(PCCM_ADDTYPE(PCCT_INT, PCCTM_FTN), PCCTM_PTR));
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
	ptype = NIL;
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
			p2 = p1->type;
			if (p2 == NLNIL || p2->chain == NLNIL || p2->chain->class != CRANGE) {
			    if (q != p2) {
				error("Parameter type not identical to type of var parameter %s of %s", p1->symbol, p->symbol);
				chk = FALSE;
			    }
			    break;
			} else {
			    /* conformant array */
			    if (p1 == ptype) {
				if (q != ctype) {
				    error("Conformant array parameters in the same specification must be the same type.");
				    goto conf_err;
				}
			    } else {
				if (classify(q) != TARY && classify(q) != TSTR) {
				    error("Array type required for var parameter %s of %s",p1->symbol,p->symbol);
				    goto conf_err;
				}
				/* check base type of array */
				if (p2->type != q->type) {
				    error("Base type of array not identical to that of conformant array parameter %s of %s", p1->symbol, p->symbol);
				    goto conf_err;
				}
				if (p2->value[0] != q->value[0]) {
				    error("Subscript number mismatch on conformant array parameter %s of %s", p1->symbol, p->symbol);
				    /* Don't process array bounds & width */
conf_err:			    if (p1->chain->type->class == CRANGE) {
					d = p1->value[0];
					for (i = 1; i <= d; i++) {
					    /* for each subscript, pass by
					     * bounds and width
					     */
					    p1 = p1->chain->chain->chain;
					}
				    }
				    ptype = ctype = NLNIL;
				    chk = FALSE;
				    break;
				}
				/*
				 * Save array type for all parameters with same
				 * specification.
				 */
				ctype = q;
				ptype = p2;
				/*
				 * If at end of conformant array list,
				 * get bounds.
				 */
				if (p1->chain->type->class == CRANGE) {
				    /* check each subscript, put on stack */
				    d = ptype->value[0];
				    q = ctype;
				    for (i = 1; i <= d; i++) {
					p1 = p1->chain;
					q = q->chain;
					if (incompat(q, p1->type, TR_NIL)){
					    error("Subscript type not conformable with parameter %s of %s", p1->symbol, p->symbol);
					    chk = FALSE;
					    break;
					}
					/* Put lower and upper bound & width */
#					ifdef OBJ
					if (q->type->class == CRANGE) {
					    putcbnds(q->type);
					} else {
					    put(2, width(p1->type) <= 2 ? O_CON2
						: O_CON4, q->range[0]);
					    put(2, width(p1->type) <= 2 ? O_CON2
						: O_CON4, q->range[1]);
					    put(2, width(p1->type) <= 2 ? O_CON2
						: O_CON4, aryconst(ctype,i));
					}
#					endif OBJ
#					ifdef PC
					if (q->type->class == CRANGE) {
					    for (j = 1; j <= 3; j++) {
						p2 = p->nptr[j];
						putRV(p2->symbol, (p2->nl_block
						    & 037), p2->value[0],
						    p2->extra_flags,p2type(p2));
						putop(PCC_CM, PCCT_INT);
					    }
					} else {
					    putleaf(PCC_ICON, q->range[0], 0,PCCT_INT,0);
					    putop( PCC_CM , PCCT_INT );
					    putleaf(PCC_ICON, q->range[1], 0,PCCT_INT,0);
					    putop( PCC_CM , PCCT_INT );
					    putleaf(PCC_ICON,aryconst(ctype,i),0,PCCT_INT,0);
					    putop( PCC_CM , PCCT_INT );
					}
#					endif PC
					p1 = p1->chain->chain;
				    }
				}
			    }
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
				    sconv(p2type(q), PCCT_DOUBLE);
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
					putstrop( PCC_STARG
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
			putop( PCC_CM , PCCT_INT );
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
		(void) put(2, O_FRTN, roundup(width(p->type), (long) A_STACK));
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
			tempdescrp -> extra_flags , PCCTM_PTR|PCCT_STRTY );
		if ( !noarguments ) {
		    putop( PCC_CM , PCCT_INT );
		}
		noarguments = FALSE;
 		putLV((char *) 0 , cbn , savedispnp -> value[ NL_OFFS ] ,
 			savedispnp -> extra_flags , PCCTM_PTR | PCCT_STRTY );
 		putop( PCC_CM , PCCT_INT );
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
			putop( ( noarguments ? PCCOM_UNARY PCC_CALL : PCC_CALL ) ,
				(int) p_type_p2type );
			if ( p -> class == FFUNC ) {
			    putop( PCC_ASSIGN , (int) p_type_p2type );
			}
			break;
		    default:
			putstrop( ( noarguments ? PCCOM_UNARY PCC_STCALL : PCC_STCALL ),
				(int) PCCM_ADDTYPE( p_type_p2type , PCCTM_PTR ) ,
				(int) p_type_width ,(int) p_type_align );
			putstrop(PCC_STASG, (int) PCCM_ADDTYPE(p_type_p2type, PCCTM_PTR),
				(int) lwidth(p -> type), align(p -> type));
			break;
		}
	    } else {
		putop( ( noarguments ? PCCOM_UNARY PCC_CALL : PCC_CALL ) , PCCT_INT );
	    }
		/*
		 *	( t=p , ... , FRTN( t ) ...
		 */
	    if ( p -> class == FFUNC || p -> class == FPROC ) {
		putop( PCC_COMOP , PCCT_INT );
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR ) ,
			"_FRTN" );
		putRV((char *) 0 , cbn , tempdescrp -> value[ NL_OFFS ] ,
			tempdescrp -> extra_flags , PCCTM_PTR | PCCT_STRTY );
 		putLV((char *) 0 , cbn , savedispnp -> value[ NL_OFFS ] ,
 			savedispnp -> extra_flags , PCCTM_PTR | PCCT_STRTY );
 		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		putop( PCC_COMOP , PCCT_INT );
	    }
		/*
		 *	if required:
		 *	either	... , temp )
		 *	or	... , &temp )
		 */
	    if ( porf == FUNC && temptype != PCCT_UNDEF ) {
		if ( temptype != PCCT_STRTY ) {
		    putRV((char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
			    tempnlp -> extra_flags , (int) p_type_p2type );
		} else {
		    putLV((char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
			    tempnlp -> extra_flags , (int) p_type_p2type );
		}
		putop( PCC_COMOP , PCCT_INT );
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
