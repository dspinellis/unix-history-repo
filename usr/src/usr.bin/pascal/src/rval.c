/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)rval.c	5.1 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
#   include	"pc.h"
#   include <pcc.h>
#endif PC
#include "tmps.h"
#include "tree_ty.h"

extern	char *opnames[];

    /* line number of the last record comparison warning */
short reccompline = 0;
    /* line number of the last non-standard set comparison */
short nssetline = 0;

#ifdef PC
    char	*relts[] =  {
				"_RELEQ" , "_RELNE" ,
				"_RELTLT" , "_RELTGT" ,
				"_RELTLE" , "_RELTGE"
			    };
    char	*relss[] =  {
				"_RELEQ" , "_RELNE" ,
				"_RELSLT" , "_RELSGT" ,
				"_RELSLE" , "_RELSGE"
			    };
    long	relops[] =  {	
				PCC_EQ , PCC_NE ,
				PCC_LT , PCC_GT ,
				PCC_LE , PCC_GE 
			    };
    long	mathop[] =  {	PCC_MUL , PCC_PLUS , PCC_MINUS };
    char	*setop[] =  {	"_MULT" , "_ADDT" , "_SUBT" };
#endif PC
/*
 * Rvalue - an expression.
 *
 * Contype is the type that the caller would prefer, nand is important
 * if constant strings are involved, because of string padding.
 * required is a flag whether an lvalue or an rvalue is required.
 * only VARs and structured things can have gt their lvalue this way.
 */
/*ARGSUSED*/
struct nl *
rvalue(r, contype , required )
	struct tnode *r;
	struct nl *contype;
	int	required;
{
	register struct nl *p, *p1;
	register struct nl *q;
	int c, c1, w;
#ifdef OBJ
	int g;
#endif
	struct tnode *rt;
	char *cp, *cp1, *opname;
	long l;
	union
	{
	    long plong[2];
	    double pdouble;
	}f;
	extern int	flagwas;
	struct csetstr	csetd;
#	ifdef PC
	    struct nl	*rettype;
	    long	ctype;
	    struct nl	*tempnlp;
#	endif PC

	if (r == TR_NIL)
		return (NLNIL);
	if (nowexp(r))
		return (NLNIL);
	/*
	 * Pick up the name of the operation
	 * for future error messages.
	 */
	if (r->tag <= T_IN)
		opname = opnames[r->tag];

	/*
	 * The root of the tree tells us what sort of expression we have.
	 */
	switch (r->tag) {

	/*
	 * The constant nil
	 */
	case T_NIL:
#		ifdef OBJ
		    (void) put(2, O_CON2, 0);
#		endif OBJ
#		ifdef PC
		    putleaf( PCC_ICON , 0 , 0 , PCCTM_PTR|PCCT_UNDEF , (char *) 0 );
#		endif PC
		return (nl+TNIL);

	/*
	 * Function call with arguments.
	 */
	case T_FCALL:
#	    ifdef OBJ
		return (funccod(r));
#	    endif OBJ
#	    ifdef PC
		return (pcfunccod( r ));
#	    endif PC

	case T_VAR:
		p = lookup(r->var_node.cptr);
		if (p == NLNIL || p->class == BADUSE)
			return (NLNIL);
		switch (p->class) {
		    case VAR:
			    /*
			     * If a variable is
			     * qualified then get
			     * the rvalue by a
			     * lvalue and an ind.
			     */
			    if (r->var_node.qual != TR_NIL)
				    goto ind;
			    q = p->type;
			    if (q == NIL)
				    return (NLNIL);
#			    ifdef OBJ
				w = width(q);
				switch (w) {
				    case 8:
					(void) put(2, O_RV8 | bn << 8+INDX,
						(int)p->value[0]);
					break;
				    case 4:
					(void) put(2, O_RV4 | bn << 8+INDX,
						(int)p->value[0]);
					break;
				    case 2:
					(void) put(2, O_RV2 | bn << 8+INDX,
						(int)p->value[0]);
					break;
				    case 1:
					(void) put(2, O_RV1 | bn << 8+INDX,
						(int)p->value[0]);
					break;
				    default:
					(void) put(3, O_RV | bn << 8+INDX,
						(int)p->value[0], w);
				}
#			   endif OBJ
#			   ifdef PC
				if ( required == RREQ ) {
				    putRV( p -> symbol , bn , p -> value[0] ,
					    p -> extra_flags , p2type( q ) );
				} else {
				    putLV( p -> symbol , bn , p -> value[0] ,
					    p -> extra_flags , p2type( q ) );
				}
#			   endif PC
			   return (q);

		    case WITHPTR:
		    case REF:
			    /*
			     * A lvalue for these
			     * is actually what one
			     * might consider a rvalue.
			     */
ind:
			    q = lvalue(r, NOFLAGS , LREQ );
			    if (q == NIL)
				    return (NLNIL);
#			    ifdef OBJ
				w = width(q);
				switch (w) {
				    case 8:
					    (void) put(1, O_IND8);
					    break;
				    case 4:
					    (void) put(1, O_IND4);
					    break;
				    case 2:
					    (void) put(1, O_IND2);
					    break;
				    case 1:
					    (void) put(1, O_IND1);
					    break;
				    default:
					    (void) put(2, O_IND, w);
				}
#			    endif OBJ
#			    ifdef PC
				if ( required == RREQ ) {
				    putop( PCCOM_UNARY PCC_MUL , p2type( q ) );
				}
#			    endif PC
			    return (q);

		    case CONST:
			    if (r->var_node.qual != TR_NIL) {
				error("%s is a constant and cannot be qualified", r->var_node.cptr);
				return (NLNIL);
			    }
			    q = p->type;
			    if (q == NLNIL)
				    return (NLNIL);
			    if (q == nl+TSTR) {
				    /*
				     * Find the size of the string
				     * constant if needed.
				     */
				    cp = (char *) p->ptr[0];
cstrng:
				    cp1 = cp;
				    for (c = 0; *cp++; c++)
					    continue;
				    w = c;
				    if (contype != NIL && !opt('s')) {
					    if (width(contype) < c && classify(contype) == TSTR) {
						    error("Constant string too long");
						    return (NLNIL);
					    }
					    w = width(contype);
				    }
#				    ifdef OBJ
					(void) put(2, O_CONG, w);
					putstr(cp1, w - c);
#				    endif OBJ
#				    ifdef PC
					putCONG( cp1 , w , required );
#				    endif PC
				    /*
				     * Define the string temporarily
				     * so later people can know its
				     * width.
				     * cleaned out by stat.
				     */
				    q = defnl((char *) 0, STR, NLNIL, w);
				    q->type = q;
				    return (q);
			    }
			    if (q == nl+T1CHAR) {
#				    ifdef OBJ
					(void) put(2, O_CONC, (int)p->value[0]);
#				    endif OBJ
#				    ifdef PC
					putleaf( PCC_ICON , p -> value[0] , 0
						, PCCT_CHAR , (char *) 0 );
#				    endif PC
				    return (q);
			    }
			    /*
			     * Every other kind of constant here
			     */
			    switch (width(q)) {
			    case 8:
#ifndef DEBUG
#				    ifdef OBJ
					(void) put(2, O_CON8, p->real);
#				    endif OBJ
#				    ifdef PC
					putCON8( p -> real );
#				    endif PC
#else
				    if (hp21mx) {
					    f.pdouble = p->real;
					    conv((int *) (&f.pdouble));
					    l = f.plong[1];
					    (void) put(2, O_CON4, l);
				    } else
#					    ifdef OBJ
						(void) put(2, O_CON8, p->real);
#					    endif OBJ
#					    ifdef PC
						putCON8( p -> real );
#					    endif PC
#endif
				    break;
			    case 4:
#				    ifdef OBJ
					(void) put(2, O_CON4, p->range[0]);
#				    endif OBJ
#				    ifdef PC
					putleaf( PCC_ICON , (int) p->range[0] , 0
						, PCCT_INT , (char *) 0 );
#				    endif PC
				    break;
			    case 2:
#				    ifdef OBJ
					(void) put(2, O_CON2, (short)p->range[0]);
#				    endif OBJ
#				    ifdef PC
					putleaf( PCC_ICON , (short) p -> range[0]
						, 0 , PCCT_SHORT , (char *) 0 );
#				    endif PC
				    break;
			    case 1:
#				    ifdef OBJ
					(void) put(2, O_CON1, p->value[0]);
#				    endif OBJ
#				    ifdef PC
					putleaf( PCC_ICON , p -> value[0] , 0
						, PCCT_CHAR , (char *) 0 );
#				    endif PC
				    break;
			    default:
				    panic("rval");
			    }
			    return (q);

		    case FUNC:
		    case FFUNC:
			    /*
			     * Function call with no arguments.
			     */
			    if (r->var_node.qual != TR_NIL) {
				    error("Can't qualify a function result value");
				    return (NLNIL);
			    }
#			    ifdef OBJ
				return (funccod(r));
#			    endif OBJ
#			    ifdef PC
				return (pcfunccod( r ));
#			    endif PC

		    case TYPE:
			    error("Type names (e.g. %s) allowed only in declarations", p->symbol);
			    return (NLNIL);

		    case PROC:
		    case FPROC:
			    error("Procedure %s found where expression required", p->symbol);
			    return (NLNIL);
		    default:
			    panic("rvid");
		}
	/*
	 * Constant sets
	 */
	case T_CSET:
#		ifdef OBJ
		    if ( precset( r , contype , &csetd ) ) {
			if ( csetd.csettype == NIL ) {
			    return (NLNIL);
			}
			postcset( r , &csetd );
		    } else {
			(void) put( 2, O_PUSH, -lwidth(csetd.csettype));
			postcset( r , &csetd );
			setran( ( csetd.csettype ) -> type );
			(void) put( 2, O_CON24, set.uprbp);
			(void) put( 2, O_CON24, set.lwrb);
			(void) put( 2, O_CTTOT,
				(int)(4 + csetd.singcnt + 2 * csetd.paircnt));
		    }
		    return csetd.csettype;
#		endif OBJ
#		ifdef PC
		    if ( precset( r , contype , &csetd ) ) {
			if ( csetd.csettype == NIL ) {
			    return (NLNIL);
			}
			postcset( r , &csetd );
		    } else {
			putleaf( PCC_ICON , 0 , 0
				, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				, "_CTTOT" );
			/*
			 *	allocate a temporary and use it
			 */
			tempnlp = tmpalloc(lwidth(csetd.csettype),
				csetd.csettype, NOREG);
			putLV( (char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
				tempnlp -> extra_flags , PCCTM_PTR|PCCT_STRTY );
			setran( ( csetd.csettype ) -> type );
			putleaf( PCC_ICON , set.lwrb , 0 , PCCT_INT , (char *) 0 );
			putop( PCC_CM , PCCT_INT );
			putleaf( PCC_ICON , set.uprbp , 0 , PCCT_INT , (char *) 0 );
			putop( PCC_CM , PCCT_INT );
			postcset( r , &csetd );
			putop( PCC_CALL , PCCT_INT );
		    }
		    return csetd.csettype;
#		endif PC

	/*
	 * Unary plus and minus
	 */
	case T_PLUS:
	case T_MINUS:
		q = rvalue(r->un_expr.expr, NLNIL , RREQ );
		if (q == NLNIL)
			return (NLNIL);
		if (isnta(q, "id")) {
			error("Operand of %s must be integer or real, not %s", opname, nameof(q));
			return (NLNIL);
		}
		if (r->tag == T_MINUS) {
#		    ifdef OBJ
			(void) put(1, O_NEG2 + (width(q) >> 2));
			return (isa(q, "d") ? q : nl+T4INT);
#		    endif OBJ
#		    ifdef PC
			if (isa(q, "i")) {
			    sconv(p2type(q), PCCT_INT);
			    putop( PCCOM_UNARY PCC_MINUS, PCCT_INT);
			    return nl+T4INT;
			}
			putop( PCCOM_UNARY PCC_MINUS, PCCT_DOUBLE);
			return nl+TDOUBLE;
#		    endif PC
		}
		return (q);

	case T_NOT:
		q = rvalue(r->un_expr.expr, NLNIL , RREQ );
		if (q == NLNIL)
			return (NLNIL);
		if (isnta(q, "b")) {
			error("not must operate on a Boolean, not %s", nameof(q));
			return (NLNIL);
		}
#		ifdef OBJ
		    (void) put(1, O_NOT);
#		endif OBJ
#		ifdef PC
		    sconv(p2type(q), PCCT_INT);
		    putop( PCC_NOT , PCCT_INT);
		    sconv(PCCT_INT, p2type(q));
#		endif PC
		return (nl+T1BOOL);

	case T_AND:
	case T_OR:
		p = rvalue(r->expr_node.lhs, NLNIL , RREQ );
#		ifdef PC
		    sconv(p2type(p),PCCT_INT);
#		endif PC
		p1 = rvalue(r->expr_node.rhs, NLNIL , RREQ );
#		ifdef PC
		    sconv(p2type(p1),PCCT_INT);
#		endif PC
		if (p == NLNIL || p1 == NLNIL)
			return (NLNIL);
		if (isnta(p, "b")) {
			error("Left operand of %s must be Boolean, not %s", opname, nameof(p));
			return (NLNIL);
		}
		if (isnta(p1, "b")) {
			error("Right operand of %s must be Boolean, not %s", opname, nameof(p1));
			return (NLNIL);
		}
#		ifdef OBJ
		    (void) put(1, r->tag == T_AND ? O_AND : O_OR);
#		endif OBJ
#		ifdef PC
			/*
			 * note the use of & and | rather than && and ||
			 * to force evaluation of all the expressions.
			 */
		    putop( r->tag == T_AND ? PCC_AND : PCC_OR , PCCT_INT );
		    sconv(PCCT_INT, p2type(p));
#		endif PC
		return (nl+T1BOOL);

	case T_DIVD:
#		ifdef OBJ
		    p = rvalue(r->expr_node.lhs, NLNIL , RREQ );
		    p1 = rvalue(r->expr_node.rhs, NLNIL , RREQ );
#		endif OBJ
#		ifdef PC
			/*
			 *	force these to be doubles for the divide
			 */
		    p = rvalue( r->expr_node.lhs , NLNIL , RREQ );
		    sconv(p2type(p), PCCT_DOUBLE);
		    p1 = rvalue( r->expr_node.rhs , NLNIL , RREQ );
		    sconv(p2type(p1), PCCT_DOUBLE);
#		endif PC
		if (p == NLNIL || p1 == NLNIL)
			return (NLNIL);
		if (isnta(p, "id")) {
			error("Left operand of / must be integer or real, not %s", nameof(p));
			return (NLNIL);
		}
		if (isnta(p1, "id")) {
			error("Right operand of / must be integer or real, not %s", nameof(p1));
			return (NLNIL);
		}
#		ifdef OBJ
		    return gen(NIL, r->tag, width(p), width(p1));
#		endif OBJ
#		ifdef PC
		    putop( PCC_DIV , PCCT_DOUBLE );
		    return nl + TDOUBLE;
#		endif PC

	case T_MULT:
	case T_ADD:
	case T_SUB:
#		ifdef OBJ
		    /*
		     * get the type of the right hand side.
		     * if it turns out to be a set,
		     * use that type when getting
		     * the type of the left hand side.
		     * and then use the type of the left hand side
		     * when generating code.
		     * this will correctly decide the type of any
		     * empty sets in the tree, since if the empty set 
		     * is on the left hand side it will inherit
		     * the type of the right hand side,
		     * and if it's on the right hand side, its type (intset)
		     * will be overridden by the type of the left hand side.
		     * this is an awful lot of tree traversing, 
		     * but it works.
		     */
		    codeoff();
		    p1 = rvalue( r->expr_node.rhs , NLNIL , RREQ );
		    codeon();
		    if ( p1 == NLNIL ) {
			return NLNIL;
		    }
		    if (isa(p1, "t")) {
			codeoff();
			contype = rvalue(r->expr_node.lhs, p1, RREQ);
			codeon();
			if (contype == NLNIL) {
			    return NLNIL;
			}
		    }
		    p = rvalue( r->expr_node.lhs , contype , RREQ );
		    p1 = rvalue( r->expr_node.rhs , p , RREQ );
		    if ( p == NLNIL || p1 == NLNIL )
			    return NLNIL;
		    if (isa(p, "id") && isa(p1, "id"))
			return (gen(NIL, r->tag, width(p), width(p1)));
		    if (isa(p, "t") && isa(p1, "t")) {
			    if (p != p1) {
				    error("Set types of operands of %s must be identical", opname);
				    return (NLNIL);
			    }
			    (void) gen(TSET, r->tag, width(p), 0);
			    return (p);
		    }
#		endif OBJ
#		ifdef PC
			/*
			 * the second pass can't do
			 *	long op double  or  double op long
			 * so we have to know the type of both operands.
			 * also, see the note for obj above on determining
			 * the type of empty sets.
			 */
		    codeoff();
		    p1 = rvalue(r->expr_node.rhs, NLNIL, RREQ);
		    codeon();
		    if ( isa( p1 , "id" ) ) {
			p = rvalue( r->expr_node.lhs , contype , RREQ );
			if ( ( p == NLNIL ) || ( p1 == NLNIL ) ) {
			    return NLNIL;
			}
			tuac(p, p1, &rettype, (int *) (&ctype));
			p1 = rvalue( r->expr_node.rhs , contype , RREQ );
			tuac(p1, p, &rettype, (int *) (&ctype));
			if ( isa( p , "id" ) ) {
			    putop( (int) mathop[r->tag - T_MULT], (int) ctype);
			    return rettype;
			}
		    }
		    if ( isa( p1 , "t" ) ) {
			putleaf( PCC_ICON , 0 , 0
			    , PCCM_ADDTYPE( PCCM_ADDTYPE( PCCTM_PTR | PCCT_STRTY , PCCTM_FTN )
					, PCCTM_PTR )
			    , setop[ r->tag - T_MULT ] );
			codeoff();
			contype = rvalue( r->expr_node.lhs, p1 , LREQ );
			codeon();
			    return NLNIL;
			}
			    /*
			     *	allocate a temporary and use it
			     */
			tempnlp = tmpalloc(lwidth(contype), contype, NOREG);
			putLV((char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
				tempnlp -> extra_flags , PCCTM_PTR|PCCT_STRTY );
			p = rvalue( r->expr_node.lhs , contype , LREQ );
			if ( isa( p , "t" ) ) {
			    putop( PCC_CM , PCCT_INT );
			    if ( p == NLNIL || p1 == NLNIL ) {
				return NLNIL;
			    }
			    p1 = rvalue( r->expr_node.rhs , p , LREQ );
			    if ( p != p1 ) {
				error("Set types of operands of %s must be identical", opname);
				return NLNIL;
			    }
			    putop( PCC_CM , PCCT_INT );
			    putleaf( PCC_ICON , (int) (lwidth(p1)) / sizeof( long ) , 0
				    , PCCT_INT , (char *) 0 );
			    putop( PCC_CM , PCCT_INT );
			    putop( PCC_CALL , PCCTM_PTR | PCCT_STRTY );
			    return p;
			}
		    }
		    if ( isnta( p1 , "idt" ) ) {
			    /*
			     *	find type of left operand for error message.
			     */
			p = rvalue( r->expr_node.lhs , contype , RREQ );
		    }
			/*
			 *	don't give spurious error messages.
			 */
		    if ( p == NLNIL || p1 == NLNIL ) {
			return NLNIL;
		    }
#		endif PC
		if (isnta(p, "idt")) {
			error("Left operand of %s must be integer, real or set, not %s", opname, nameof(p));
			return (NLNIL);
		}
		if (isnta(p1, "idt")) {
			error("Right operand of %s must be integer, real or set, not %s", opname, nameof(p1));
			return (NLNIL);
		}
		error("Cannot mix sets with integers and reals as operands of %s", opname);
		return (NLNIL);

	case T_MOD:
	case T_DIV:
		p = rvalue(r->expr_node.lhs, NLNIL , RREQ );
#		ifdef PC
		    sconv(p2type(p), PCCT_INT);
#		endif PC
		p1 = rvalue(r->expr_node.rhs, NLNIL , RREQ );
#		ifdef PC
		    sconv(p2type(p1), PCCT_INT);
#		endif PC
		if (p == NLNIL || p1 == NLNIL)
			return (NLNIL);
		if (isnta(p, "i")) {
			error("Left operand of %s must be integer, not %s", opname, nameof(p));
			return (NLNIL);
		}
		if (isnta(p1, "i")) {
			error("Right operand of %s must be integer, not %s", opname, nameof(p1));
			return (NLNIL);
		}
#		ifdef OBJ
		    return (gen(NIL, r->tag, width(p), width(p1)));
#		endif OBJ
#		ifdef PC
		    putop( r->tag == T_DIV ? PCC_DIV : PCC_MOD , PCCT_INT );
		    return ( nl + T4INT );
#		endif PC

	case T_EQ:
	case T_NE:
	case T_LT:
	case T_GT:
	case T_LE:
	case T_GE:
		/*
		 * Since there can be no, a priori, knowledge
		 * of the context type should a constant string
		 * or set arise, we must poke around to find such
		 * a type if possible.  Since constant strings can
		 * always masquerade as identifiers, this is always
		 * necessary.
		 * see the note in the obj section of case T_MULT above
		 * for the determination of the base type of empty sets.
		 */
		codeoff();
		p1 = rvalue(r->expr_node.rhs, NLNIL , RREQ );
		codeon();
		if (p1 == NLNIL)
			return (NLNIL);
		contype = p1;
#		ifdef OBJ
		    if (p1->class == STR) {
			    /*
			     * For constant strings we want
			     * the longest type so as to be
			     * able to do padding (more importantly
			     * avoiding truncation). For clarity,
			     * we get this length here.
			     */
			    codeoff();
			    p = rvalue(r->expr_node.lhs, NLNIL , RREQ );
			    codeon();
			    if (p == NLNIL)
				    return (NLNIL);
			    if (width(p) > width(p1))
				    contype = p;
		    }
		    if (isa(p1, "t")) {
			codeoff();
			contype = rvalue(r->expr_node.lhs, p1, RREQ);
			codeon();
			if (contype == NLNIL) {
			    return NLNIL;
			}
		    }
		    /*
		     * Now we generate code for
		     * the operands of the relational
		     * operation.
		     */
		    p = rvalue(r->expr_node.lhs, contype , RREQ );
		    if (p == NLNIL)
			    return (NLNIL);
		    p1 = rvalue(r->expr_node.rhs, p , RREQ );
		    if (p1 == NLNIL)
			    return (NLNIL);
#		endif OBJ
#		ifdef PC
		    c1 = classify( p1 );
		    if ( c1 == TSET || c1 == TSTR || c1 == TREC ) {
			putleaf( PCC_ICON , 0 , 0
				, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				, c1 == TSET  ? relts[ r->tag - T_EQ ]
					      : relss[ r->tag - T_EQ ] );
			    /*
			     *	for [] and strings, comparisons are done on
			     *	the maximum width of the two sides.
			     *	for other sets, we have to ask the left side
			     *	what type it is based on the type of the right.
			     *	(this matters for intsets).
			     */
			if ( c1 == TSTR ) {
			    codeoff();
			    p = rvalue( r->expr_node.lhs , NLNIL , LREQ );
			    codeon();
			    if ( p == NLNIL ) {
				return NLNIL;
			    }
			    if ( lwidth( p ) > lwidth( p1 ) ) {
				contype = p;
			    }
			} else if ( c1 == TSET ) {
			    codeoff();
			    contype = rvalue(r->expr_node.lhs, p1, LREQ);
			    codeon();
			    if (contype == NLNIL) {
				return NLNIL;
			    }
			} 
			    /*
			     *	put out the width of the comparison.
			     */
			putleaf(PCC_ICON, (int) lwidth(contype), 0, PCCT_INT, (char *) 0);
			    /*
			     *	and the left hand side,
			     *	for sets, strings, records
			     */
			p = rvalue( r->expr_node.lhs , contype , LREQ );
			if ( p == NLNIL ) {
			    return NLNIL;
			}
			putop( PCC_CM , PCCT_INT );
			p1 = rvalue( r->expr_node.rhs , p , LREQ );
			if ( p1 == NLNIL ) {
			    return NLNIL;
			}
			putop( PCC_CM , PCCT_INT );
			putop( PCC_CALL , PCCT_INT );
		    } else {
			    /*
			     *	the easy (scalar or error) case
			     */
			p = rvalue( r->expr_node.lhs , contype , RREQ );
			if ( p == NLNIL ) {
			    return NLNIL;
			}
			    /*
			     * since the second pass can't do
			     *	long op double  or  double op long
			     * we may have to do some coercing.
			     */
			tuac(p, p1, &rettype, (int *) (&ctype));
			p1 = rvalue( r->expr_node.rhs , p , RREQ );
			if ( p1 == NLNIL ) {
			    return NLNIL;
			}
			tuac(p1, p, &rettype, (int *) (&ctype));
			putop((int) relops[ r->tag - T_EQ ] , PCCT_INT );
			sconv(PCCT_INT, PCCT_CHAR);
		    }
#		endif PC
		c = classify(p);
		c1 = classify(p1);
		if (nocomp(c) || nocomp(c1))
			return (NLNIL);
#		ifdef OBJ
		    g = NIL;
#		endif
		switch (c) {
			case TBOOL:
			case TCHAR:
				if (c != c1)
					goto clash;
				break;
			case TINT:
			case TDOUBLE:
				if (c1 != TINT && c1 != TDOUBLE)
					goto clash;
				break;
			case TSCAL:
				if (c1 != TSCAL)
					goto clash;
				if (scalar(p) != scalar(p1))
					goto nonident;
				break;
			case TSET:
				if (c1 != TSET)
					goto clash;
				if ( opt( 's' ) &&
				    ( ( r->tag == T_LT) || (r->tag == T_GT) ) &&
				    ( line != nssetline ) ) {
				    nssetline = line;
				    standard();
				    error("%s comparison on sets is non-standard" , opname );
				}
				if (p != p1)
					goto nonident;
#				ifdef OBJ
				    g = TSET;
#				endif
				break;
			case TREC:
				if ( c1 != TREC ) {
				    goto clash;
				}
				if ( p != p1 ) {
				    goto nonident;
				}
				if (r->tag != T_EQ && r->tag != T_NE) {
					error("%s not allowed on records - only allow = and <>" , opname );
					return (NLNIL);
				}
#				ifdef OBJ
				    g = TREC;
#				endif
				break;
			case TPTR:
			case TNIL:
				if (c1 != TPTR && c1 != TNIL)
					goto clash;
				if (r->tag != T_EQ && r->tag != T_NE) {
					error("%s not allowed on pointers - only allow = and <>" , opname );
					return (NLNIL);
				}
				if (p != nl+TNIL && p1 != nl+TNIL && p != p1)
					goto nonident;
				break;
			case TSTR:
				if (c1 != TSTR)
					goto clash;
				if (width(p) != width(p1)) {
					error("Strings not same length in %s comparison", opname);
					return (NLNIL);
				}
#				ifdef OBJ
				    g = TSTR;
#				endif OBJ
				break;
			default:
				panic("rval2");
		}
#		ifdef OBJ
		    return (gen(g, r->tag, width(p), width(p1)));
#		endif OBJ
#		ifdef PC
		    return nl + TBOOL;
#		endif PC
clash:
		error("%ss and %ss cannot be compared - operator was %s", clnames[c], clnames[c1], opname);
		return (NLNIL);
nonident:
		error("%s types must be identical in comparisons - operator was %s", clnames[c1], opname);
		return (NLNIL);

	case T_IN:
	    rt = r->expr_node.rhs;
#	    ifdef OBJ
		if (rt != TR_NIL && rt->tag == T_CSET) {
			(void) precset( rt , NLNIL , &csetd );
			p1 = csetd.csettype;
			if (p1 == NLNIL)
			    return NLNIL;
			postcset( rt, &csetd);
		    } else {
			p1 = stkrval(r->expr_node.rhs, NLNIL , (long) RREQ );
			rt = TR_NIL;
		    }
#		endif OBJ
#		ifdef PC
		    if (rt != TR_NIL && rt->tag == T_CSET) {
			if ( precset( rt , NLNIL , &csetd ) ) {
			    putleaf( PCC_ICON , 0 , 0
				    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				    , "_IN" );
			} else {
			    putleaf( PCC_ICON , 0 , 0
				    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				    , "_INCT" );
			}
			p1 = csetd.csettype;
			if (p1 == NIL)
			    return NLNIL;
		    } else {
			putleaf( PCC_ICON , 0 , 0
				, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				, "_IN" );
			codeoff();
			p1 = rvalue(r->expr_node.rhs, NLNIL , LREQ );
			codeon();
		    }
#		endif PC
		p = stkrval(r->expr_node.lhs, NLNIL , (long) RREQ );
		if (p == NIL || p1 == NIL)
			return (NLNIL);
		if (p1->class != (char) SET) {
			error("Right operand of 'in' must be a set, not %s", nameof(p1));
			return (NLNIL);
		}
		if (incompat(p, p1->type, r->expr_node.lhs)) {
			cerror("Index type clashed with set component type for 'in'");
			return (NLNIL);
		}
		setran(p1->type);
#		ifdef OBJ
		    if (rt == TR_NIL || csetd.comptime)
			    (void) put(4, O_IN, width(p1), set.lwrb, set.uprbp);
		    else
			    (void) put(2, O_INCT,
				(int)(3 + csetd.singcnt + 2*csetd.paircnt));
#		endif OBJ
#		ifdef PC
		    if ( rt == TR_NIL || rt->tag != T_CSET ) {
			putleaf( PCC_ICON , set.lwrb , 0 , PCCT_INT , (char *) 0 );
			putop( PCC_CM , PCCT_INT );
			putleaf( PCC_ICON , set.uprbp , 0 , PCCT_INT , (char *) 0 );
			putop( PCC_CM , PCCT_INT );
			p1 = rvalue( r->expr_node.rhs , NLNIL , LREQ );
			if ( p1 == NLNIL ) {
			    return NLNIL;
			}
			putop( PCC_CM , PCCT_INT );
		    } else if ( csetd.comptime ) {
			putleaf( PCC_ICON , set.lwrb , 0 , PCCT_INT , (char *) 0 );
			putop( PCC_CM , PCCT_INT );
			putleaf( PCC_ICON , set.uprbp , 0 , PCCT_INT , (char *) 0 );
			putop( PCC_CM , PCCT_INT );
			postcset( r->expr_node.rhs , &csetd );
			putop( PCC_CM , PCCT_INT );
		    } else {
			postcset( r->expr_node.rhs , &csetd );
		    }
		    putop( PCC_CALL , PCCT_INT );
		    sconv(PCCT_INT, PCCT_CHAR);
#		endif PC
		return (nl+T1BOOL);
	default:
		if (r->expr_node.lhs == TR_NIL)
			return (NLNIL);
		switch (r->tag) {
		default:
			panic("rval3");


		/*
		 * An octal number
		 */
		case T_BINT:
			f.pdouble = a8tol(r->const_node.cptr);
			goto conint;
	
		/*
		 * A decimal number
		 */
		case T_INT:
			f.pdouble = atof(r->const_node.cptr);
conint:
			if (f.pdouble > MAXINT || f.pdouble < MININT) {
				error("Constant too large for this implementation");
				return (NLNIL);
			}
			l = f.pdouble;
#			ifdef OBJ
			    if (bytes(l, l) <= 2) {
				    (void) put(2, O_CON2, ( short ) l);
				    return (nl+T2INT);
			    }
			    (void) put(2, O_CON4, l); 
			    return (nl+T4INT);
#			endif OBJ
#			ifdef PC
			    switch (bytes(l, l)) {
				case 1:
				    putleaf(PCC_ICON, (int) l, 0, PCCT_CHAR, 
						(char *) 0);
				    return nl+T1INT;
				case 2:
				    putleaf(PCC_ICON, (int) l, 0, PCCT_SHORT, 
						(char *) 0);
				    return nl+T2INT;
				case 4:
				    putleaf(PCC_ICON, (int) l, 0, PCCT_INT,
						(char *) 0);
				    return nl+T4INT;
			    }
#			endif PC
	
		/*
		 * A floating point number
		 */
		case T_FINT:
#			ifdef OBJ
			    (void) put(2, O_CON8, atof(r->const_node.cptr));
#			endif OBJ
#			ifdef PC
			    putCON8( atof( r->const_node.cptr ) );
#			endif PC
			return (nl+TDOUBLE);
	
		/*
		 * Constant strings.  Note that constant characters
		 * are constant strings of length one; there is
		 * no constant string of length one.
		 */
		case T_STRNG:
			cp = r->const_node.cptr;
			if (cp[1] == 0) {
#				ifdef OBJ
				    (void) put(2, O_CONC, cp[0]);
#				endif OBJ
#				ifdef PC
				    putleaf( PCC_ICON , cp[0] , 0 , PCCT_CHAR ,
						(char *) 0 );
#				endif PC
				return (nl+T1CHAR);
			}
			goto cstrng;
		}
	
	}
}

/*
 * Can a class appear
 * in a comparison ?
 */
nocomp(c)
	int c;
{

	switch (c) {
		case TREC:
			if ( line != reccompline ) {
			    reccompline = line;
			    warning();
			    if ( opt( 's' ) ) {
				standard();
			    }
			    error("record comparison is non-standard");
			}
			break;
		case TFILE:
		case TARY:
			error("%ss may not participate in comparisons", clnames[c]);
			return (1);
	}
	return (NIL);
}

    /*
     *	this is sort of like gconst, except it works on expression trees
     *	rather than declaration trees, and doesn't give error messages for
     *	non-constant things.
     *	as a side effect this fills in the con structure that gconst uses.
     *	this returns TRUE or FALSE.
     */

bool 
constval(r)
	register struct tnode *r;
{
	register struct nl *np;
	register struct tnode *cn;
	char *cp;
	int negd, sgnd;
	long ci;

	con.ctype = NIL;
	cn = r;
	negd = sgnd = 0;
loop:
	    /*
	     *	cn[2] is nil if error recovery generated a T_STRNG
	     */
	if (cn == TR_NIL || cn->expr_node.lhs == TR_NIL)
		return FALSE;
	switch (cn->tag) {
		default:
			return FALSE;
		case T_MINUS:
			negd = 1 - negd;
			/* and fall through */
		case T_PLUS:
			sgnd++;
			cn = cn->un_expr.expr;
			goto loop;
		case T_NIL:
			con.cpval = NIL;
			con.cival = 0;
			con.crval = con.cival;
			con.ctype = nl + TNIL;
			break;
		case T_VAR:
			np = lookup(cn->var_node.cptr);
			if (np == NLNIL || np->class != CONST) {
				return FALSE;
			}
			if ( cn->var_node.qual != TR_NIL ) {
				return FALSE;
			}
			con.ctype = np->type;
			switch (classify(np->type)) {
				case TINT:
					con.crval = np->range[0];
					break;
				case TDOUBLE:
					con.crval = np->real;
					break;
				case TBOOL:
				case TCHAR:
				case TSCAL:
					con.cival = np->value[0];
					con.crval = con.cival;
					break;
				case TSTR:
					con.cpval = (char *) np->ptr[0];
					break;
				default:
					con.ctype = NIL;
					return FALSE;
			}
			break;
		case T_BINT:
			con.crval = a8tol(cn->const_node.cptr);
			goto restcon;
		case T_INT:
			con.crval = atof(cn->const_node.cptr);
			if (con.crval > MAXINT || con.crval < MININT) {
				derror("Constant too large for this implementation");
				con.crval = 0;
			}
restcon:
			ci = con.crval;
#ifndef PI0
			if (bytes(ci, ci) <= 2)
				con.ctype = nl+T2INT;
			else	
#endif
				con.ctype = nl+T4INT;
			break;
		case T_FINT:
			con.ctype = nl+TDOUBLE;
			con.crval = atof(cn->const_node.cptr);
			break;
		case T_STRNG:
			cp = cn->const_node.cptr;
			if (cp[1] == 0) {
				con.ctype = nl+T1CHAR;
				con.cival = cp[0];
				con.crval = con.cival;
				break;
			}
			con.ctype = nl+TSTR;
			con.cpval = cp;
			break;
	}
	if (sgnd) {
		if (isnta(con.ctype, "id")) {
			derror("%s constants cannot be signed", nameof(con.ctype));
			return FALSE;
		} else if (negd)
			con.crval = -con.crval;
	}
	return TRUE;
}
