/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)rval.c 1.13 %G%";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
#   include	"pc.h"
#   include "pcops.h"
#endif PC

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
				P2EQ , P2NE ,
				P2LT , P2GT ,
				P2LE , P2GE 
			    };
    long	mathop[] =  {	P2MUL , P2PLUS , P2MINUS };
    char	*setop[] =  {	"_MULT" , "_ADDT" , "_SUBT" };
#endif PC
/*
 * Rvalue - an expression.
 *
 * Contype is the type that the caller would prefer, nand is important
 * if constant sets or constant strings are involved, the latter
 * because of string padding.
 * required is a flag whether an lvalue or an rvalue is required.
 * only VARs and structured things can have gt their lvalue this way.
 */
struct nl *
rvalue(r, contype , required )
	int *r;
	struct nl *contype;
	int	required;
{
	register struct nl *p, *p1;
	register struct nl *q;
	int c, c1, *rt, w, g;
	char *cp, *cp1, *opname;
	long l;
	double f;
	extern int	flagwas;
	struct csetstr	csetd;
#	ifdef PC
	    struct nl	*rettype;
	    long	ctype;
	    struct nl	*tempnlp;
#	endif PC

	if (r == NIL)
		return (NIL);
	if (nowexp(r))
		return (NIL);
	/*
	 * Pick up the name of the operation
	 * for future error messages.
	 */
	if (r[0] <= T_IN)
		opname = opnames[r[0]];

	/*
	 * The root of the tree tells us what sort of expression we have.
	 */
	switch (r[0]) {

	/*
	 * The constant nil
	 */
	case T_NIL:
#		ifdef OBJ
		    put(2, O_CON2, 0);
#		endif OBJ
#		ifdef PC
		    putleaf( P2ICON , 0 , 0 , P2PTR|P2UNDEF , 0 );
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
		p = lookup(r[2]);
		if (p == NIL || p->class == BADUSE)
			return (NIL);
		switch (p->class) {
		    case VAR:
			    /*
			     * If a variable is
			     * qualified then get
			     * the rvalue by a
			     * lvalue and an ind.
			     */
			    if (r[3] != NIL)
				    goto ind;
			    q = p->type;
			    if (q == NIL)
				    return (NIL);
#			    ifdef OBJ
				w = width(q);
				switch (w) {
				    case 8:
					put(2, O_RV8 | bn << 8+INDX,
						(int)p->value[0]);
					break;
				    case 4:
					put(2, O_RV4 | bn << 8+INDX,
						(int)p->value[0]);
					break;
				    case 2:
					put(2, O_RV2 | bn << 8+INDX,
						(int)p->value[0]);
					break;
				    case 1:
					put(2, O_RV1 | bn << 8+INDX,
						(int)p->value[0]);
					break;
				    default:
					put(3, O_RV | bn << 8+INDX,
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
				    return (NIL);
#			    ifdef OBJ
				w = width(q);
				switch (w) {
				    case 8:
					    put(1, O_IND8);
					    break;
				    case 4:
					    put(1, O_IND4);
					    break;
				    case 2:
					    put(1, O_IND2);
					    break;
				    case 1:
					    put(1, O_IND1);
					    break;
				    default:
					    put(2, O_IND, w);
				}
#			    endif OBJ
#			    ifdef PC
				if ( required == RREQ ) {
				    putop( P2UNARY P2MUL , p2type( q ) );
				}
#			    endif PC
			    return (q);

		    case CONST:
			    if (r[3] != NIL) {
				error("%s is a constant and cannot be qualified", r[2]);
				return (NIL);
			    }
			    q = p->type;
			    if (q == NIL)
				    return (NIL);
			    if (q == nl+TSTR) {
				    /*
				     * Find the size of the string
				     * constant if needed.
				     */
				    cp = p->ptr[0];
cstrng:
				    cp1 = cp;
				    for (c = 0; *cp++; c++)
					    continue;
				    w = c;
				    if (contype != NIL && !opt('s')) {
					    if (width(contype) < c && classify(contype) == TSTR) {
						    error("Constant string too long");
						    return (NIL);
					    }
					    w = width(contype);
				    }
#				    ifdef OBJ
					put(2, O_CONG, w);
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
				    q = defnl(0, STR, 0, w);
				    q->type = q;
				    return (q);
			    }
			    if (q == nl+T1CHAR) {
#				    ifdef OBJ
					put(2, O_CONC, (int)p->value[0]);
#				    endif OBJ
#				    ifdef PC
					putleaf( P2ICON , p -> value[0] , 0
						, P2CHAR , 0 );
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
					put(2, O_CON8, p->real);
#				    endif OBJ
#				    ifdef PC
					putCON8( p -> real );
#				    endif PC
#else
				    if (hp21mx) {
					    f = p->real;
					    conv(&f);
					    l = f.plong;
					    put(2, O_CON4, l);
				    } else
#					    ifdef OBJ
						put(2, O_CON8, p->real);
#					    endif OBJ
#					    ifdef PC
						putCON8( p -> real );
#					    endif PC
#endif
				    break;
			    case 4:
#				    ifdef OBJ
					put(2, O_CON4, p->range[0]);
#				    endif OBJ
#				    ifdef PC
					putleaf( P2ICON , p -> range[0] , 0
						, P2INT , 0 );
#				    endif PC
				    break;
			    case 2:
#				    ifdef OBJ
					put(2, O_CON2, (short)p->range[0]);
#				    endif OBJ
#				    ifdef PC
					    /*
					     * make short constants ints
					     */
					putleaf( P2ICON , (short) p -> range[0]
						, 0 , P2INT , 0 );
#				    endif PC
				    break;
			    case 1:
#				    ifdef OBJ
					put(2, O_CON1, p->value[0]);
#				    endif OBJ
#				    ifdef PC
					    /*
					     * make char constants ints
					     */
					putleaf( P2ICON , p -> value[0] , 0
						, P2INT , 0 );
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
			    if (r[3]) {
				    error("Can't qualify a function result value");
				    return (NIL);
			    }
#			    ifdef OBJ
				return (funccod((int *) r));
#			    endif OBJ
#			    ifdef PC
				return (pcfunccod( r ));
#			    endif PC

		    case TYPE:
			    error("Type names (e.g. %s) allowed only in declarations", p->symbol);
			    return (NIL);

		    case PROC:
		    case FPROC:
			    error("Procedure %s found where expression required", p->symbol);
			    return (NIL);
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
			    return NIL;
			}
			postcset( r , &csetd );
		    } else {
			put( 2, O_PUSH, -lwidth(csetd.csettype));
			postcset( r , &csetd );
			setran( ( csetd.csettype ) -> type );
			put( 2, O_CON24, set.uprbp);
			put( 2, O_CON24, set.lwrb);
			put( 2, O_CTTOT,
				(int)(4 + csetd.singcnt + 2 * csetd.paircnt));
		    }
		    return csetd.csettype;
#		endif OBJ
#		ifdef PC
		    if ( precset( r , contype , &csetd ) ) {
			if ( csetd.csettype == NIL ) {
			    return NIL;
			}
			postcset( r , &csetd );
		    } else {
			putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR )
				, "_CTTOT" );
			/*
			 *	allocate a temporary and use it
			 */
			tempnlp = tmpalloc(lwidth(csetd.csettype),
				csetd.csettype, NOREG);
			putLV( 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
				tempnlp -> extra_flags , P2PTR|P2STRTY );
			setran( ( csetd.csettype ) -> type );
			putleaf( P2ICON , set.lwrb , 0 , P2INT , 0 );
			putop( P2LISTOP , P2INT );
			putleaf( P2ICON , set.uprbp , 0 , P2INT , 0 );
			putop( P2LISTOP , P2INT );
			postcset( r , &csetd );
			putop( P2CALL , P2INT );
		    }
		    return csetd.csettype;
#		endif PC

	/*
	 * Unary plus and minus
	 */
	case T_PLUS:
	case T_MINUS:
		q = rvalue(r[2], NIL , RREQ );
		if (q == NIL)
			return (NIL);
		if (isnta(q, "id")) {
			error("Operand of %s must be integer or real, not %s", opname, nameof(q));
			return (NIL);
		}
		if (r[0] == T_MINUS) {
#		    ifdef OBJ
			put(1, O_NEG2 + (width(q) >> 2));
#		    endif OBJ
#		    ifdef PC
			putop( P2UNARY P2MINUS , p2type( q ) );
#		    endif PC
		    return (isa(q, "d") ? q : nl+T4INT);
		}
		return (q);

	case T_NOT:
		q = rvalue(r[2], NIL , RREQ );
		if (q == NIL)
			return (NIL);
		if (isnta(q, "b")) {
			error("not must operate on a Boolean, not %s", nameof(q));
			return (NIL);
		}
#		ifdef OBJ
		    put(1, O_NOT);
#		endif OBJ
#		ifdef PC
		    putop( P2NOT , P2INT );
#		endif PC
		return (nl+T1BOOL);

	case T_AND:
	case T_OR:
		p = rvalue(r[2], NIL , RREQ );
		p1 = rvalue(r[3], NIL , RREQ );
		if (p == NIL || p1 == NIL)
			return (NIL);
		if (isnta(p, "b")) {
			error("Left operand of %s must be Boolean, not %s", opname, nameof(p));
			return (NIL);
		}
		if (isnta(p1, "b")) {
			error("Right operand of %s must be Boolean, not %s", opname, nameof(p1));
			return (NIL);
		}
#		ifdef OBJ
		    put(1, r[0] == T_AND ? O_AND : O_OR);
#		endif OBJ
#		ifdef PC
			/*
			 * note the use of & and | rather than && and ||
			 * to force evaluation of all the expressions.
			 */
		    putop( r[ 0 ] == T_AND ? P2AND : P2OR , P2INT );
#		endif PC
		return (nl+T1BOOL);

	case T_DIVD:
#		ifdef OBJ
		    p = rvalue(r[2], NIL , RREQ );
		    p1 = rvalue(r[3], NIL , RREQ );
#		endif OBJ
#		ifdef PC
			/*
			 *	force these to be doubles for the divide
			 */
		    p = rvalue( r[ 2 ] , NIL , RREQ );
		    if ( isnta( p , "d" ) ) {
			putop( P2SCONV , P2DOUBLE );
		    }
		    p1 = rvalue( r[ 3 ] , NIL , RREQ );
		    if ( isnta( p1 , "d" ) ) {
			putop( P2SCONV , P2DOUBLE );
		    }
#		endif PC
		if (p == NIL || p1 == NIL)
			return (NIL);
		if (isnta(p, "id")) {
			error("Left operand of / must be integer or real, not %s", nameof(p));
			return (NIL);
		}
		if (isnta(p1, "id")) {
			error("Right operand of / must be integer or real, not %s", nameof(p1));
			return (NIL);
		}
#		ifdef OBJ
		    return gen(NIL, r[0], width(p), width(p1));
#		endif OBJ
#		ifdef PC
		    putop( P2DIV , P2DOUBLE );
		    return nl + TDOUBLE;
#		endif PC

	case T_MULT:
	case T_ADD:
	case T_SUB:
#		ifdef OBJ
		    /*
		     * If the context hasn't told us the type
		     * and a constant set is present
		     * we need to infer the type 
		     * before generating code.
		     */
		    if ( contype == NIL ) {
			    codeoff();
			    contype = rvalue( r[3] , NIL , RREQ );
			    codeon();
			    if ( contype == lookup( intset ) -> type ) {
				codeoff();
				contype = rvalue( r[2] , NIL , RREQ );
				codeon();
			    }
		    }
		    if ( contype == NIL ) {
			return NIL;
		    }
		    p = rvalue( r[2] , contype , RREQ );
		    p1 = rvalue( r[3] , p , RREQ );
		    if ( p == NIL || p1 == NIL )
			    return NIL;
		    if (isa(p, "id") && isa(p1, "id"))
			return (gen(NIL, r[0], width(p), width(p1)));
		    if (isa(p, "t") && isa(p1, "t")) {
			    if (p != p1) {
				    error("Set types of operands of %s must be identical", opname);
				    return (NIL);
			    }
			    gen(TSET, r[0], width(p), 0);
			    return (p);
		    }
#		endif OBJ
#		ifdef PC
			/*
			 * the second pass can't do
			 *	long op double  or  double op long
			 * so we have to know the type of both operands
			 * also, it gets tricky for sets, which are done
			 * by function calls.
			 */
		    codeoff();
		    p1 = rvalue( r[ 3 ] , contype , RREQ );
		    codeon();
		    if ( isa( p1 , "id" ) ) {
			p = rvalue( r[ 2 ] , contype , RREQ );
			if ( ( p == NIL ) || ( p1 == NIL ) ) {
			    return NIL;
			}
			if ( isa( p , "i" ) && isa( p1 , "d" ) ) {
			    putop( P2SCONV , P2DOUBLE );
			}
			p1 = rvalue( r[ 3 ] , contype , RREQ );
			if ( isa( p , "d" ) && isa( p1 , "i" ) ) {
			    putop( P2SCONV , P2DOUBLE );
			}
			if ( isa( p , "id" ) ) {
			    if ( isa( p , "d" ) || isa( p1 , "d" ) ) {
				ctype = P2DOUBLE;
				rettype = nl + TDOUBLE;
			    } else {
				ctype = P2INT;
				rettype = nl + T4INT;
			    }
			    putop( mathop[ r[0] - T_MULT ] , ctype );
			    return rettype;
			}
		    }
		    if ( isa( p1 , "t" ) ) {
			putleaf( P2ICON , 0 , 0
			    , ADDTYPE( ADDTYPE( P2PTR | P2STRTY , P2FTN )
					, P2PTR )
			    , setop[ r[0] - T_MULT ] );
			if ( contype == NIL ) {
			    contype = p1;
			    if ( contype == lookup( intset ) -> type ) {
				codeoff();
				contype = rvalue( r[2] , NIL , LREQ );
				codeon();
			    }
			}
			if ( contype == NIL ) {
			    return NIL;
			}
			    /*
			     *	allocate a temporary and use it
			     */
			tempnlp = tmpalloc(lwidth(contype), contype, NOREG);
			putLV( 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
				tempnlp -> extra_flags , P2PTR|P2STRTY );
			p = rvalue( r[2] , contype , LREQ );
			if ( isa( p , "t" ) ) {
			    putop( P2LISTOP , P2INT );
			    if ( p == NIL || p1 == NIL ) {
				return NIL;
			    }
			    p1 = rvalue( r[3] , p , LREQ );
			    if ( p != p1 ) {
				error("Set types of operands of %s must be identical", opname);
				return NIL;
			    }
			    putop( P2LISTOP , P2INT );
			    putleaf( P2ICON , lwidth( p1 ) / sizeof( long ) , 0
				    , P2INT , 0 );
			    putop( P2LISTOP , P2INT );
			    putop( P2CALL , P2PTR | P2STRTY );
			    return p;
			}
		    }
		    if ( isnta( p1 , "idt" ) ) {
			    /*
			     *	find type of left operand for error message.
			     */
			p = rvalue( r[2] , contype , RREQ );
		    }
			/*
			 *	don't give spurious error messages.
			 */
		    if ( p == NIL || p1 == NIL ) {
			return NIL;
		    }
#		endif PC
		if (isnta(p, "idt")) {
			error("Left operand of %s must be integer, real or set, not %s", opname, nameof(p));
			return (NIL);
		}
		if (isnta(p1, "idt")) {
			error("Right operand of %s must be integer, real or set, not %s", opname, nameof(p1));
			return (NIL);
		}
		error("Cannot mix sets with integers and reals as operands of %s", opname);
		return (NIL);

	case T_MOD:
	case T_DIV:
		p = rvalue(r[2], NIL , RREQ );
		p1 = rvalue(r[3], NIL , RREQ );
		if (p == NIL || p1 == NIL)
			return (NIL);
		if (isnta(p, "i")) {
			error("Left operand of %s must be integer, not %s", opname, nameof(p));
			return (NIL);
		}
		if (isnta(p1, "i")) {
			error("Right operand of %s must be integer, not %s", opname, nameof(p1));
			return (NIL);
		}
#		ifdef OBJ
		    return (gen(NIL, r[0], width(p), width(p1)));
#		endif OBJ
#		ifdef PC
		    putop( r[ 0 ] == T_DIV ? P2DIV : P2MOD , P2INT );
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
		 */
		codeoff();
		p1 = rvalue(r[3], NIL , RREQ );
		codeon();
		if (p1 == NIL)
			return (NIL);
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
			    p = rvalue(r[2], NIL , RREQ );
			    codeon();
			    if (p == NIL)
				    return (NIL);
			    if (width(p) > width(p1))
				    contype = p;
		    } else if ( isa( p1 , "t" ) ) {
			if ( contype == lookup( intset ) -> type ) {
			    codeoff();
			    contype = rvalue( r[2] , NIL , RREQ );
			    codeon();
			    if ( contype == NIL ) {
				return NIL;
			    }
			}
		    }
		    /*
		     * Now we generate code for
		     * the operands of the relational
		     * operation.
		     */
		    p = rvalue(r[2], contype , RREQ );
		    if (p == NIL)
			    return (NIL);
		    p1 = rvalue(r[3], p , RREQ );
		    if (p1 == NIL)
			    return (NIL);
#		endif OBJ
#		ifdef PC
		    c1 = classify( p1 );
		    if ( c1 == TSET || c1 == TSTR || c1 == TREC ) {
			putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR )
				, c1 == TSET  ? relts[ r[0] - T_EQ ]
					      : relss[ r[0] - T_EQ ] );
			    /*
			     *	for [] and strings, comparisons are done on
			     *	the maximum width of the two sides.
			     *	for other sets, we have to ask the left side
			     *	what type it is based on the type of the right.
			     *	(this matters for intsets).
			     */
			if ( c1 == TSTR ) {
			    codeoff();
			    p = rvalue( r[ 2 ] , NIL , LREQ );
			    codeon();
			    if ( p == NIL ) {
				return NIL;
			    }
			    if ( lwidth( p ) > lwidth( p1 ) ) {
				contype = p;
			    }
			} else if ( c1 == TSET ) {
			    if ( contype == lookup( intset ) -> type ) {
				codeoff();
				p = rvalue( r[ 2 ] , NIL , LREQ );
				codeon();
				if ( p == NIL ) {
				    return NIL;
				}
				contype = p;
			    }
			} 
			    /*
			     *	put out the width of the comparison.
			     */
			putleaf( P2ICON , lwidth( contype ) , 0 , P2INT , 0 );
			    /*
			     *	and the left hand side,
			     *	for sets, strings, records
			     */
			p = rvalue( r[ 2 ] , contype , LREQ );
			if ( p == NIL ) {
			    return NIL;
			}
			putop( P2LISTOP , P2INT );
			p1 = rvalue( r[ 3 ] , p , LREQ );
			if ( p1 == NIL ) {
			    return NIL;
			}
			putop( P2LISTOP , P2INT );
			putop( P2CALL , P2INT );
		    } else {
			    /*
			     *	the easy (scalar or error) case
			     */
			p = rvalue( r[ 2 ] , contype , RREQ );
			if ( p == NIL ) {
			    return NIL;
			}
			    /*
			     * since the second pass can't do
			     *	long op double  or  double op long
			     * we may have to do some coercing.
			     */
			if ( isa( p , "i" ) && isa( p1 , "d" ) ) {
			    putop( P2SCONV , P2DOUBLE );
			}
			p1 = rvalue( r[ 3 ] , p , RREQ );
			if ( p1 == NIL ) {
			    return NIL;
			}
			if ( isa( p , "d" ) && isa( p1 , "i" ) )
			    putop( P2SCONV , P2DOUBLE );
			putop( relops[ r[0] - T_EQ ] , P2INT );
		    }
#		endif PC
		c = classify(p);
		c1 = classify(p1);
		if (nocomp(c) || nocomp(c1))
			return (NIL);
		g = NIL;
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
				    ( ( r[0] == T_LT ) || ( r[0] == T_GT ) ) &&
				    ( line != nssetline ) ) {
				    nssetline = line;
				    standard();
				    error("%s comparison on sets is non-standard" , opname );
				}
				if (p != p1)
					goto nonident;
				g = TSET;
				break;
			case TREC:
				if ( c1 != TREC ) {
				    goto clash;
				}
				if ( p != p1 ) {
				    goto nonident;
				}
				if (r[0] != T_EQ && r[0] != T_NE) {
					error("%s not allowed on records - only allow = and <>" , opname );
					return (NIL);
				}
				g = TREC;
				break;
			case TPTR:
			case TNIL:
				if (c1 != TPTR && c1 != TNIL)
					goto clash;
				if (r[0] != T_EQ && r[0] != T_NE) {
					error("%s not allowed on pointers - only allow = and <>" , opname );
					return (NIL);
				}
				break;
			case TSTR:
				if (c1 != TSTR)
					goto clash;
				if (width(p) != width(p1)) {
					error("Strings not same length in %s comparison", opname);
					return (NIL);
				}
				g = TSTR;
				break;
			default:
				panic("rval2");
		}
#		ifdef OBJ
		    return (gen(g, r[0], width(p), width(p1)));
#		endif OBJ
#		ifdef PC
		    return nl + TBOOL;
#		endif PC
clash:
		error("%ss and %ss cannot be compared - operator was %s", clnames[c], clnames[c1], opname);
		return (NIL);
nonident:
		error("%s types must be identical in comparisons - operator was %s", clnames[c1], opname);
		return (NIL);

	case T_IN:
	    rt = r[3];
#	    ifdef OBJ
		if (rt != NIL && rt[0] == T_CSET) {
			precset( rt , NIL , &csetd );
			p1 = csetd.csettype;
			if (p1 == NIL)
			    return NIL;
			postcset( rt, &csetd);
		    } else {
			p1 = stkrval(r[3], NIL , RREQ );
			rt = NIL;
		    }
#		endif OBJ
#		ifdef PC
		    if (rt != NIL && rt[0] == T_CSET) {
			if ( precset( rt , NIL , &csetd ) ) {
			    putleaf( P2ICON , 0 , 0
				    , ADDTYPE( P2FTN | P2INT , P2PTR )
				    , "_IN" );
			} else {
			    putleaf( P2ICON , 0 , 0
				    , ADDTYPE( P2FTN | P2INT , P2PTR )
				    , "_INCT" );
			}
			p1 = csetd.csettype;
			if (p1 == NIL)
			    return NIL;
		    } else {
			putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR )
				, "_IN" );
			codeoff();
			p1 = rvalue(r[3], NIL , LREQ );
			codeon();
		    }
#		endif PC
		p = stkrval(r[2], NIL , RREQ );
		if (p == NIL || p1 == NIL)
			return (NIL);
		if (p1->class != SET) {
			error("Right operand of 'in' must be a set, not %s", nameof(p1));
			return (NIL);
		}
		if (incompat(p, p1->type, r[2])) {
			cerror("Index type clashed with set component type for 'in'");
			return (NIL);
		}
		setran(p1->type);
#		ifdef OBJ
		    if (rt == NIL || csetd.comptime)
			    put(4, O_IN, width(p1), set.lwrb, set.uprbp);
		    else
			    put(2, O_INCT,
				(int)(3 + csetd.singcnt + 2*csetd.paircnt));
#		endif OBJ
#		ifdef PC
		    if ( rt == NIL || rt[0] != T_CSET ) {
			putleaf( P2ICON , set.lwrb , 0 , P2INT , 0 );
			putop( P2LISTOP , P2INT );
			putleaf( P2ICON , set.uprbp , 0 , P2INT , 0 );
			putop( P2LISTOP , P2INT );
			p1 = rvalue( r[3] , NIL , LREQ );
			if ( p1 == NIL ) {
			    return NIL;
			}
			putop( P2LISTOP , P2INT );
		    } else if ( csetd.comptime ) {
			putleaf( P2ICON , set.lwrb , 0 , P2INT , 0 );
			putop( P2LISTOP , P2INT );
			putleaf( P2ICON , set.uprbp , 0 , P2INT , 0 );
			putop( P2LISTOP , P2INT );
			postcset( r[3] , &csetd );
			putop( P2LISTOP , P2INT );
		    } else {
			postcset( r[3] , &csetd );
		    }
		    putop( P2CALL , P2INT );
#		endif PC
		return (nl+T1BOOL);
	default:
		if (r[2] == NIL)
			return (NIL);
		switch (r[0]) {
		default:
			panic("rval3");


		/*
		 * An octal number
		 */
		case T_BINT:
			f = a8tol(r[2]);
			goto conint;
	
		/*
		 * A decimal number
		 */
		case T_INT:
			f = atof(r[2]);
conint:
			if (f > MAXINT || f < MININT) {
				error("Constant too large for this implementation");
				return (NIL);
			}
			l = f;
			if (bytes(l, l) <= 2) {
#				ifdef OBJ
				    put(2, O_CON2, ( short ) l);
#				endif OBJ
#				ifdef PC
				        /*
					 * short constants are ints
					 */
				    putleaf( P2ICON , l , 0 , P2INT , 0 );
#				endif PC
				return (nl+T2INT);
			}
#			ifdef OBJ
			    put(2, O_CON4, l); 
#			endif OBJ
#			ifdef PC
			    putleaf( P2ICON , l , 0 , P2INT , 0 );
#			endif PC
			return (nl+T4INT);
	
		/*
		 * A floating point number
		 */
		case T_FINT:
#			ifdef OBJ
			    put(2, O_CON8, atof(r[2]));
#			endif OBJ
#			ifdef PC
			    putCON8( atof( r[2] ) );
#			endif PC
			return (nl+TDOUBLE);
	
		/*
		 * Constant strings.  Note that constant characters
		 * are constant strings of length one; there is
		 * no constant string of length one.
		 */
		case T_STRNG:
			cp = r[2];
			if (cp[1] == 0) {
#				ifdef OBJ
				    put(2, O_CONC, cp[0]);
#				endif OBJ
#				ifdef PC
				    putleaf( P2ICON , cp[0] , 0 , P2CHAR , 0 );
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
constval(r)
	register int *r;
{
	register struct nl *np;
	register *cn;
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
	if (cn == NIL || cn[2] == NIL)
		return FALSE;
	switch (cn[0]) {
		default:
			return FALSE;
		case T_MINUS:
			negd = 1 - negd;
			/* and fall through */
		case T_PLUS:
			sgnd++;
			cn = cn[2];
			goto loop;
		case T_NIL:
			con.cpval = NIL;
			con.cival = 0;
			con.crval = con.cival;
			con.ctype = nl + TNIL;
			break;
		case T_VAR:
			np = lookup(cn[2]);
			if (np == NIL || np->class != CONST) {
				return FALSE;
			}
			if ( cn[3] != NIL ) {
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
					con.cpval = np->ptr[0];
					break;
				default:
					con.ctype = NIL;
					return FALSE;
			}
			break;
		case T_BINT:
			con.crval = a8tol(cn[2]);
			goto restcon;
		case T_INT:
			con.crval = atof(cn[2]);
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
			con.crval = atof(cn[2]);
			break;
		case T_STRNG:
			cp = cn[2];
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
