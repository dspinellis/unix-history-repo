/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)lval.c 1.1 8/27/80";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
#   include	"pc.h"
#   include	"pcops.h"
#endif PC

extern	int flagwas;
/*
 * Lvalue computes the address
 * of a qualified name and
 * leaves it on the stack.
 * for pc, it can be asked for either an lvalue or an rvalue.
 * the semantics are the same, only the code is different.
 */
struct nl *
lvalue(r, modflag , required )
	int *r, modflag;
	int	required;
{
	register struct nl *p;
	struct nl *firstp, *lastp;
	register *c, *co;
	int f, o;
	/*
	 * Note that the local optimizations
	 * done here for offsets would more
	 * appropriately be done in put.
	 */
	int tr[2], trp[3];

	if (r == NIL) {
		return (NIL);
	}
	if (nowexp(r)) {
		return (NIL);
	}
	if (r[0] != T_VAR) {
		error("Variable required");	/* Pass mesgs down from pt of call ? */
		return (NIL);
	}
#	ifdef PC
		/*
		 *	pc requires a whole different control flow
		 */
	    return pclvalue( r , modflag , required );
#	endif PC
	firstp = p = lookup(r[2]);
	if (p == NIL) {
		return (NIL);
	}
	c = r[3];
	if ((modflag & NOUSE) && !lptr(c)) {
		p->nl_flags = flagwas;
	}
	if (modflag & MOD) {
		p->nl_flags |= NMOD;
	}
	/*
	 * Only possibilities for p->class here
	 * are the named classes, i.e. CONST, TYPE
	 * VAR, PROC, FUNC, REF, or a WITHPTR.
	 */
	switch (p->class) {
		case WITHPTR:
			/*
			 * Construct the tree implied by
			 * the with statement
			 */
			trp[0] = T_LISTPP;
			trp[1] = tr;
			trp[2] = r[3];
			tr[0] = T_FIELD;
			tr[1] = r[2];
			c = trp;
#			ifdef PTREE
			    /*
			     * mung r[4] to say which field this T_VAR is
			     * for VarCopy
			     */
			    r[4] = reclook( p -> type , r[2] );
#			endif
			/* and fall through */
		case REF:
			/*
			 * Obtain the indirect word
			 * of the WITHPTR or REF
			 * as the base of our lvalue
			 */
			put(2, PTR_RV | bn << 8+INDX , p->value[0] );
			f = 0;		/* have an lv on stack */
			o = 0;
			break;
		case VAR:
			f = 1;		/* no lv on stack yet */
			o = p->value[0];
			break;
		default:
			error("%s %s found where variable required", classes[p->class], p->symbol);
			return (NIL);
	}
	/*
	 * Loop and handle each
	 * qualification on the name
	 */
	if (c == NIL && (modflag&ASGN) && p->value[NL_FORV]) {
		error("Can't modify the for variable %s in the range of the loop", p->symbol);
		return (NIL);
	}
	for (; c != NIL; c = c[2]) {
		co = c[1];
		if (co == NIL) {
			return (NIL);
		}
		lastp = p;
		p = p->type;
		if (p == NIL) {
			return (NIL);
		}
		switch (co[0]) {
			case T_PTR:
				/*
				 * Pointer qualification.
				 */
				lastp->nl_flags |= NUSED;
				if (p->class != PTR && p->class != FILET) {
					error("^ allowed only on files and pointers, not on %ss", nameof(p));
					goto bad;
				}
				if (f) {
				    put(2, PTR_RV | bn <<8+INDX , o );
				} else {
					if (o) {
					    put2(O_OFF, o);
					}
					put(1, PTR_IND);
				}
				/*
				 * Pointer cannot be
				 * nil and file cannot
				 * be at end-of-file.
				 */
				put1(p->class == FILET ? O_FNIL : O_NIL);
				f = o = 0;
				continue;
			case T_ARGL:
				if (p->class != ARRAY) {
					if (lastp == firstp) {
						error("%s is a %s, not a function", r[2], classes[firstp->class]);
					} else {
						error("Illegal function qualificiation");
					}
					return (NIL);
				}
				recovered();
				error("Pascal uses [] for subscripting, not ()");
			case T_ARY:
				if (p->class != ARRAY) {
					error("Subscripting allowed only on arrays, not on %ss", nameof(p));
					goto bad;
				}
				if (f) {
					put2(O_LV | bn<<8+INDX, o);
				} else {
					if (o) {
					    put2(O_OFF, o);
					}
				}
				switch (arycod(p, co[1])) {
					case 0:
						return (NIL);
					case -1:
						goto bad;
				}
				f = o = 0;
				continue;
			case T_FIELD:
				/*
				 * Field names are just
				 * an offset with some 
				 * semantic checking.
				 */
				if (p->class != RECORD) {
					error(". allowed only on records, not on %ss", nameof(p));
					goto bad;
				}
				if (co[1] == NIL) {
					return (NIL);
				}
				p = reclook(p, co[1]);
				if (p == NIL) {
					error("%s is not a field in this record", co[1]);
					goto bad;
				}
#				ifdef PTREE
				    /*
				     * mung co[3] to indicate which field
				     * this is for SelCopy
				     */
				    co[3] = p;
#				endif
				if (modflag & MOD) {
					p->nl_flags |= NMOD;
				}
				if ((modflag & NOUSE) == 0 || lptr(c[2])) {
					p->nl_flags |= NUSED;
				}
				o += p->value[0];
				continue;
			default:
				panic("lval2");
		}
	}
	if (f) {
		put2(O_LV | bn<<8+INDX, o);
	} else {
		if (o) {
		    put2(O_OFF, o);
		}
	}
	return (p->type);
bad:
	cerror("Error occurred on qualification of %s", r[2]);
	return (NIL);
}

lptr(c)
	register int *c;
{
	register int *co;

	for (; c != NIL; c = c[2]) {
		co = c[1];
		if (co == NIL) {
			return (NIL);
		}
		switch (co[0]) {

		case T_PTR:
			return (1);
		case T_ARGL:
			return (0);
		case T_ARY:
		case T_FIELD:
			continue;
		default:
			panic("lptr");
		}
	}
	return (0);
}

/*
 * Arycod does the
 * code generation
 * for subscripting.
 */
arycod(np, el)
	struct nl *np;
	int *el;
{
	register struct nl *p, *ap;
	int i, d, v, v1;
	int w;

	p = np;
	if (el == NIL) {
		return (0);
	}
	d = p->value[0];
	/*
	 * Check each subscript
	 */
	for (i = 1; i <= d; i++) {
		if (el == NIL) {
			error("Too few subscripts (%d given, %d required)", i-1, d);
			return (-1);
		}
		p = p->chain;
#		ifdef PC
		    precheck( p , "_SUBSC" , "_SUBSCZ" );
#		endif PC
		ap = rvalue(el[1], NLNIL , RREQ );
		if (ap == NIL) {
			return (0);
		}
#		ifdef PC
		    postcheck( p );
#		endif PC
		if (incompat(ap, p->type, el[1])) {
			cerror("Array index type incompatible with declared index type");
			if (d != 1) {
				cerror("Error occurred on index number %d", i);
			}
			return (-1);
		}
		w = aryconst(np, i);
#		ifdef OBJ
		    if (opt('t') == 0) {
			    switch (w) {
			    case 8:
				    w = 6;
			    case 4:
			    case 2:
			    case 1:
				    put2((width(ap) != 4 ? O_INX2P2 : O_INX4P2) | (w & ~1) << 7, ( short ) p->range[0]);
				    el = el[2];
				    continue;
			    }
		    }
		    put(4, width(ap) != 4 ? O_INX2 : O_INX4,w,( short ) p->range[0],
			   ( short ) ( p->range[1] - p->range[0] ) );
#		endif OBJ
#		ifdef PC
			/*
			 *	subtract off the lower bound
			 */
		    if ( p -> range[ 0 ] != 0 ) {
			putleaf( P2ICON , p -> range[0] , 0 , P2INT , 0 );
			putop( P2MINUS , P2INT );
		    }
			/*
			 *	multiply by the width of the elements
			 */
		    if ( w != 1 ) {
			putleaf( P2ICON , w , 0 , P2INT , 0 );
			putop( P2MUL , P2INT );
		    }
			/*
			 *	and add it to the base address
			 */
		    putop( P2PLUS , ADDTYPE( p2type( np -> type ) , P2PTR ) );
#		endif PC
		el = el[2];
	}
	if (el != NIL) {
		do {
			el = el[2];
			i++;
		} while (el != NIL);
		error("Too many subscripts (%d given, %d required)", i-1, d);
		return (-1);
	}
	return (1);
}
