/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)stkrval.c	5.2 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "align.h"
#ifdef PC
#   include <pcc.h>
#endif PC
#include "tree_ty.h"

/*
 * stkrval Rvalue - an expression, and coerce it to be a stack quantity.
 *
 * Contype is the type that the caller would prefer, nand is important
 * if constant sets or constant strings are involved, the latter
 * because of string padding.
 */
/*
 * for the obj version, this is a copy of rvalue hacked to use fancy new
 * push-onto-stack-and-convert opcodes.
 * for the pc version, i just call rvalue and convert if i have to,
 * based on the return type of rvalue.
 */
struct nl *
stkrval(r, contype , required )
	register struct tnode *r;
	struct nl *contype;
	long	required;
{
	register struct nl *p;
	register struct nl *q;
	register char *cp, *cp1;
	register int c, w;
	struct tnode *pt;
	long l;
	union
	{
		double pdouble;
		long   plong[2];
	}f;

	if (r == TR_NIL)
		return (NLNIL);
	if (nowexp(r))
		return (NLNIL);
	/*
	 * The root of the tree tells us what sort of expression we have.
	 */
	switch (r->tag) {

	/*
	 * The constant nil
	 */
	case T_NIL:
#		ifdef OBJ
		    (void) put(2, O_CON14, 0);
#		endif OBJ
#		ifdef PC
		    putleaf( PCC_ICON , 0 , 0 , PCCT_INT , (char *) 0 );
#		endif PC
		return (nl+TNIL);

	case T_FCALL:
	case T_VAR:
		p = lookup(r->var_node.cptr);
		if (p == NLNIL || p->class == BADUSE)
			return (NLNIL);
		switch (p->class) {
		case VAR:
			/*
			 * if a variable is
			 * qualified then get
			 * the rvalue by a
			 * stklval and an ind.
			 */
			if (r->var_node.qual != TR_NIL)
				goto ind;
			q = p->type;
			if (q == NLNIL)
				return (NLNIL);
			if (classify(q) == TSTR)
				return(stklval(r, NOFLAGS));
#			ifdef OBJ
				return (stackRV(p));
#			endif OBJ
#			ifdef PC
			    q = rvalue( r , contype , (int) required );
			    if (isa(q, "sbci")) {
				sconv(p2type(q),PCCT_INT);
			    }
			    return q;
#			endif PC

		case WITHPTR:
		case REF:
			/*
			 * A stklval for these
			 * is actually what one
			 * might consider a rvalue.
			 */
ind:
			q = stklval(r, NOFLAGS);
			if (q == NLNIL)
				return (NLNIL);
			if (classify(q) == TSTR)
				return(q);
#			ifdef OBJ
			    w = width(q);
			    switch (w) {
				    case 8:
					    (void) put(1, O_IND8);
					    return(q);
				    case 4:
					    (void) put(1, O_IND4);
					    return(q);
				    case 2:
					    (void) put(1, O_IND24);
					    return(q);
				    case 1:
					    (void) put(1, O_IND14);
					    return(q);
				    default:
					    (void) put(2, O_IND, w);
					    return(q);
			    }
#			endif OBJ
#			ifdef PC
			    if ( required == RREQ ) {
				putop( PCCOM_UNARY PCC_MUL , p2type( q ) );
				if (isa(q,"sbci")) {
				    sconv(p2type(q),PCCT_INT);
				}
			    }
			    return q;
#			endif PC

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
#				ifdef OBJ
				    (void) put(2, O_LVCON, lenstr(cp1, w - c));
				    putstr(cp1, w - c);
#				endif OBJ
#				ifdef PC
				    putCONG( cp1 , w , LREQ );
#				endif PC
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
#			    ifdef OBJ
				(void) put(2, O_CONC4, (int)p->value[0]);
#			    endif OBJ
#			    ifdef PC
				putleaf(PCC_ICON, p -> value[0], 0, PCCT_INT, 
						(char *) 0);
#			    endif PC
			    return(q);
			}
			/*
			 * Every other kind of constant here
			 */
#			ifdef OBJ
			    switch (width(q)) {
			    case 8:
#ifndef DEBUG
				    (void) put(2, O_CON8, p->real);
				    return(q);
#else
				    if (hp21mx) {
					    f.pdouble = p->real;
					    conv((int *) (&f.pdouble));
					    l = f.plong[1];
					    (void) put(2, O_CON4, l);
				    } else
					    (void) put(2, O_CON8, p->real);
				    return(q);
#endif
			    case 4:
				    (void) put(2, O_CON4, p->range[0]);
				    return(q);
			    case 2:
				    (void) put(2, O_CON24, (short)p->range[0]);
				    return(q);
			    case 1:
				    (void) put(2, O_CON14, p->value[0]);
				    return(q);
			    default:
				    panic("stkrval");
			    }
#			endif OBJ
#			ifdef PC
			    q = rvalue( r , contype , (int) required );
			    if (isa(q,"sbci")) {
				sconv(p2type(q),PCCT_INT);
			    }
			    return q;
#			endif PC

		case FUNC:
		case FFUNC:
			/*
			 * Function call
			 */
			pt = r->var_node.qual;
			if (pt != TR_NIL) {
				switch (pt->list_node.list->tag) {
				case T_PTR:
				case T_ARGL:
				case T_ARY:
				case T_FIELD:
					error("Can't qualify a function result value");
					return (NLNIL);
				}
			}
#			ifdef OBJ
			    q = p->type;
			    if (classify(q) == TSTR) {
				    c = width(q);
				    (void) put(2, O_LVCON,
					roundup(c+1, (long) A_SHORT));
				    putstr("", c);
				    (void) put(1, PTR_DUP);
				    p = funccod(r);
				    (void) put(2, O_AS, c);
				    return(p);
			    }
			    p = funccod(r);
			    if (width(p) <= 2)
				    (void) put(1, O_STOI);
#			endif OBJ
#			ifdef PC
			    p = pcfunccod( r );
			    if (isa(p,"sbci")) {
				sconv(p2type(p),PCCT_INT);
			    }
#			endif PC
			return (p);

		case TYPE:
			error("Type names (e.g. %s) allowed only in declarations", p->symbol);
			return (NLNIL);

		case PROC:
		case FPROC:
			error("Procedure %s found where expression required", p->symbol);
			return (NLNIL);
		default:
			panic("stkrvid");
		}
	case T_PLUS:
	case T_MINUS:
	case T_NOT:
	case T_AND:
	case T_OR:
	case T_DIVD:
	case T_MULT:
	case T_SUB:
	case T_ADD:
	case T_MOD:
	case T_DIV:
	case T_EQ:
	case T_NE:
	case T_GE:
	case T_LE:
	case T_GT:
	case T_LT:
	case T_IN:
		p = rvalue(r, contype , (int) required );
#		ifdef OBJ
		    if (width(p) <= 2)
			    (void) put(1, O_STOI);
#		endif OBJ
#		ifdef PC
		    if (isa(p,"sbci")) {
			sconv(p2type(p),PCCT_INT);
		    }
#		endif PC
		return (p);
	case T_CSET:
		p = rvalue(r, contype , (int) required );
		return (p);
	default:
		if (r->const_node.cptr == (char *) NIL)
			return (NLNIL);
		switch (r->tag) {
		default:
			panic("stkrval3");

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
			if (bytes(l, l) <= 2) {
#			    ifdef OBJ
				(void) put(2, O_CON24, (short)l);
#			    endif OBJ
#			    ifdef PC
				putleaf( PCC_ICON , (short) l , 0 , PCCT_INT , 
						(char *) 0 );
#			    endif PC
				return(nl+T4INT);
			}
#			ifdef OBJ
			    (void) put(2, O_CON4, l); 
#			endif OBJ
#			ifdef PC
			    putleaf( PCC_ICON , (int) l , 0 , PCCT_INT , (char *) 0 );
#			endif PC
			return (nl+T4INT);
	
		/*
		 * A floating point number
		 */
		case T_FINT:
#		   	ifdef OBJ
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
				    (void) put(2, O_CONC4, cp[0]);
#				endif OBJ
#				ifdef PC
				    putleaf( PCC_ICON , cp[0] , 0 , PCCT_INT , 
						(char *) 0 );
#				endif PC
				return(nl+T1CHAR);
			}
			goto cstrng;
		}
	
	}
}

#ifdef OBJ
/*
 * push a value onto the interpreter stack, longword aligned.
 */
struct nl 
*stackRV(p)
	struct nl *p;
{
	struct nl *q;
	int w, bn;

	q = p->type;
	if (q == NLNIL)
		return (NLNIL);
	bn = BLOCKNO(p->nl_block);
	w = width(q);
	switch (w) {
	case 8:
		(void) put(2, O_RV8 | bn << 8+INDX, (int)p->value[0]);
		break;
	case 4:
		(void) put(2, O_RV4 | bn << 8+INDX, (int)p->value[0]);
		break;
	case 2:
		(void) put(2, O_RV24 | bn << 8+INDX, (int)p->value[0]);
		break;
	case 1:
		(void) put(2, O_RV14 | bn << 8+INDX, (int)p->value[0]);
		break;
	default:
		(void) put(3, O_RV | bn << 8+INDX, (int)p->value[0], w);
		break;
	}
	return (q);
}
#endif OBJ
