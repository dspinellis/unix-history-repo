/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)lval.c	5.2 (Berkeley) 7/26/85";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "tree_ty.h"
#ifdef PC
#   include	"pc.h"
#   include	<pcc.h>
#endif PC

extern	int flagwas;
/*
 * Lvalue computes the address
 * of a qualified name and
 * leaves it on the stack.
 * for pc, it can be asked for either an lvalue or an rvalue.
 * the semantics are the same, only the code is different.
 */
/*ARGSUSED*/
struct nl *
lvalue(var, modflag , required )
	struct tnode *var; 
	int	modflag;
	int	required;
{
#ifdef OBJ
	register struct nl *p;
	struct nl *firstp, *lastp;
	register struct tnode *c, *co;
	int f, o, s;
	/*
	 * Note that the local optimizations
	 * done here for offsets would more
	 * appropriately be done in put.
	 */
	struct tnode	tr;	/* T_FIELD */ 
	struct tnode	*tr_ptr;
	struct tnode	l_node;
#endif

	if (var == TR_NIL) {
		return (NLNIL);
	}
	if (nowexp(var)) {
		return (NLNIL);
	}
	if (var->tag != T_VAR) {
		error("Variable required");	/* Pass mesgs down from pt of call ? */
		return (NLNIL);
	}
#	ifdef PC
		/*
		 *	pc requires a whole different control flow
		 */
	    return pclvalue( var , modflag , required );
#	endif PC
#	ifdef OBJ
		/*
		 *	pi uses the rest of the function
		 */
	firstp = p = lookup(var->var_node.cptr);
	if (p == NLNIL) {
		return (NLNIL);
	}
	c = var->var_node.qual;
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
	tr_ptr = &l_node;
	switch (p->class) {
		case WITHPTR:
			/*
			 * Construct the tree implied by
			 * the with statement
			 */
			l_node.tag = T_LISTPP;

			/* the cast has got to go but until the node is figured
			   out it stays */

			tr_ptr->list_node.list = (&tr);
			tr_ptr->list_node.next = var->var_node.qual;
			tr.tag = T_FIELD;
			tr.field_node.id_ptr = var->var_node.cptr;
			c = tr_ptr; /* c is a ptr to a tnode */
#			ifdef PTREE
			    /*
			     * mung var->fields to say which field this T_VAR is
			     * for VarCopy
			     */

			    /* problem! reclook returns struct nl* */

			    var->var_node.fields = reclook( p -> type , 
					    var->var_node.line_no );
#			endif
			/* and fall through */
		case REF:
			/*
			 * Obtain the indirect word
			 * of the WITHPTR or REF
			 * as the base of our lvalue
			 */
			(void) put(2, PTR_RV | bn << 8+INDX , (int)p->value[0] );
			f = 0;		/* have an lv on stack */
			o = 0;
			break;
		case VAR:
			if (p->type->class != CRANGE) {
			    f = 1;		/* no lv on stack yet */
			    o = p->value[0];
			} else {
			    error("Conformant array bound %s found where variable required", p->symbol);
			    return(NLNIL);
			}
			break;
		default:
			error("%s %s found where variable required", classes[p->class], p->symbol);
			return (NLNIL);
	}
	/*
	 * Loop and handle each
	 * qualification on the name
	 */
	if (c == TR_NIL && (modflag&ASGN) && ( p->value[NL_FORV] & FORVAR ) ) {
		error("Can't modify the for variable %s in the range of the loop", p->symbol);
		return (NLNIL);
	}
	s = 0;		/* subscripts seen */
	for (; c != TR_NIL; c = c->list_node.next) {
		co = c->list_node.list; /* co is a ptr to a tnode */
		if (co == TR_NIL) {
			return (NLNIL);
		}
		lastp = p;
		p = p->type;
		if (p == NLNIL) {
			return (NLNIL);
		}
		/*
		 * If we haven't seen enough subscripts, and the next
		 * qualification isn't array reference, then it's an error.
		 */
		if (s && co->tag != T_ARY) {
			error("Too few subscripts (%d given, %d required)",
				s, p->value[0]);
		}
		switch (co->tag) {
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
				    if (p->class == FILET && bn != 0)
				        (void) put(2, O_LV | bn <<8+INDX , o );
				    else
					/*
					 * this is the indirection from
					 * the address of the pointer 
					 * to the pointer itself.
					 * kirk sez:
					 * fnil doesn't want this.
					 * and does it itself for files
					 * since only it knows where the
					 * actual window is.
					 * but i have to do this for
					 * regular pointers.
					 * This is further complicated by
					 * the fact that global variables
					 * are referenced through pointers
					 * on the stack. Thus an RV on a
					 * global variable is the same as
					 * an LV of a non-global one ?!?
					 */
				        (void) put(2, PTR_RV | bn <<8+INDX , o );
				} else {
					if (o) {
					    (void) put(2, O_OFF, o);
					}
				        if (p->class != FILET || bn == 0)
					    (void) put(1, PTR_IND);
				}
				/*
				 * Pointer cannot be
				 * nil and file cannot
				 * be at end-of-file.
				 */
				(void) put(1, p->class == FILET ? O_FNIL : O_NIL);
				f = o = 0;
				continue;
			case T_ARGL:
				if (p->class != ARRAY) {
					if (lastp == firstp) {
						error("%s is a %s, not a function", var->var_node.cptr, classes[firstp->class]);
					} else {
						error("Illegal function qualificiation");
					}
					return (NLNIL);
				}
				recovered();
				error("Pascal uses [] for subscripting, not ()");
			case T_ARY:
				if (p->class != ARRAY) {
					error("Subscripting allowed only on arrays, not on %ss", nameof(p));
					goto bad;
				}
				if (f) {
					if (bn == 0)
						/*
						 * global variables are
						 * referenced through pointers
						 * on the stack
						 */
						(void) put(2, PTR_RV | bn<<8+INDX, o);
					else
						(void) put(2, O_LV | bn<<8+INDX, o);
				} else {
					if (o) {
					    (void) put(2, O_OFF, o);
					}
				}
				switch(s = arycod(p,co->ary_node.expr_list,s)) {
					/*
					 * This is the number of subscripts seen
					 */
					case 0:
						return (NLNIL);
					case -1:
						goto bad;
				}
				if (s == p->value[0]) {
					s = 0;
				} else {
					p = lastp;
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
				/* must define the field node!! */
				if (co->field_node.id_ptr == NIL) {
					return (NLNIL);
				}
				p = reclook(p, co->field_node.id_ptr);
				if (p == NLNIL) {
					error("%s is not a field in this record", co->field_node.id_ptr);
					goto bad;
				}
#				ifdef PTREE
				    /*
				     * mung co[3] to indicate which field
				     * this is for SelCopy
				     */
				    co->field_node.nl_entry = p;
#				endif
				if (modflag & MOD) {
					p->nl_flags |= NMOD;
				}
				if ((modflag & NOUSE) == 0 ||
				    lptr(c->list_node.next)) {
				/* figure out what kind of node c is !! */
					p->nl_flags |= NUSED;
				}
				o += p->value[0];
				continue;
			default:
				panic("lval2");
		}
	}
	if (s) {
		error("Too few subscripts (%d given, %d required)",
			s, p->type->value[0]);
		return NLNIL;
	}
	if (f) {
		if (bn == 0)
			/*
			 * global variables are referenced through
			 * pointers on the stack
			 */
			(void) put(2, PTR_RV | bn<<8+INDX, o);
		else
			(void) put(2, O_LV | bn<<8+INDX, o);
	} else {
		if (o) {
		    (void) put(2, O_OFF, o);
		}
	}
	return (p->type);
bad:
	cerror("Error occurred on qualification of %s", var->var_node.cptr);
	return (NLNIL);
#	endif OBJ
}

int lptr(c)
	register struct tnode *c;
{
	register struct tnode *co;

	for (; c != TR_NIL; c = c->list_node.next) {
		co = c->list_node.list;
		if (co == TR_NIL) {
			return (NIL);
		}
		switch (co->tag) {

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
 * n is the number of
 * subscripts already seen
 * (CLN 09/13/83)
 */
int arycod(np, el, n)
	struct nl *np;
	struct tnode *el;
	int n;
{
	register struct nl *p, *ap;
	long sub;
	bool constsub;
	extern bool constval();
	int i, d;  /* v, v1;  these aren't used */
	int w;

	p = np;
	if (el == TR_NIL) {
		return (0);
	}
	d = p->value[0];
	for (i = 1; i <= n; i++) {
		p = p->chain;
	}
	/*
	 * Check each subscript
	 */
	for (i = n+1; i <= d; i++) {
		if (el == TR_NIL) {
			return (i-1);
		}
		p = p->chain;
		if (p == NLNIL)
			return (0);
		if ((p->class != CRANGE) &&
			(constsub = constval(el->list_node.list))) {
		    ap = con.ctype;
		    sub = con.crval;
		    if (sub < p->range[0] || sub > p->range[1]) {
			error("Subscript value of %D is out of range", (char *) sub);
			return (0);
		    }
		    sub -= p->range[0];
		} else {
#		    ifdef PC
			precheck( p , "_SUBSC" , "_SUBSCZ" );
#		    endif PC
		    ap = rvalue(el->list_node.list, NLNIL , RREQ );
		    if (ap == NIL) {
			    return (0);
		    }
#		    ifdef PC
			postcheck(p, ap);
			sconv(p2type(ap),PCCT_INT);
#		    endif PC
		}
		if (incompat(ap, p->type, el->list_node.list)) {
			cerror("Array index type incompatible with declared index type");
			if (d != 1) {
				cerror("Error occurred on index number %d", (char *) i);
			}
			return (-1);
		}
		if (p->class == CRANGE) {
			constsub = FALSE;
		} else {
			w = aryconst(np, i);
		}
#		ifdef OBJ
		    if (constsub) {
			sub *= w;
			if (sub != 0) {
			    w = bytes(sub, sub);
			    (void) put(2, w <= 2 ? O_CON2 : O_CON4, sub);
			    (void) gen(NIL, T_ADD, sizeof(char *), w);
			}
			el = el->list_node.next;
			continue;
		    }
		    if (p->class == CRANGE) {
			putcbnds(p, 0);
			putcbnds(p, 1);
			putcbnds(p, 2);
		    } else if (opt('t') == 0) {
			    switch (w) {
			    case 8:
				    w = 6;
			    case 4:
			    case 2:
			    case 1:
				    (void) put(2, (width(ap) != 4 ? O_INX2P2 : O_INX4P2) | (w & ~1) << 7, ( short ) p->range[0]);
				    el = el->list_node.next;
				    continue;
			    }
		    }
		    if (p->class == CRANGE) {
			if (width(p) == 4) {
			    put(1, width(ap) != 4 ? O_VINX42 : O_VINX4);
			} else {
			    put(1, width(ap) != 4 ? O_VINX2 : O_VINX24);
			}
		    } else {
			put(4, width(ap) != 4 ? O_INX2 : O_INX4, w,
			    (short)p->range[0], (short)(p->range[1]));
		    }
		    el = el->list_node.next;
		    continue;
#		endif OBJ
#		ifdef PC
			/*
			 *	subtract off the lower bound
			 */
		    if (constsub) {
			sub *= w;
			if (sub != 0) {
			    putleaf( PCC_ICON , (int) sub , 0 , PCCT_INT , (char *) 0 );
			    putop(PCC_PLUS, PCCM_ADDTYPE(p2type(np->type), PCCTM_PTR));
			}
			el = el->list_node.next;
			continue;
		    }
		    if (p->class == CRANGE) {
			/*
			 *	if conformant array, subtract off lower bound
			 */
			ap = p->nptr[0];
			putRV(ap->symbol, (ap->nl_block & 037), ap->value[0], 
				ap->extra_flags, p2type( ap ) );
			putop( PCC_MINUS, PCCT_INT );
			/*
			 *	and multiply by the width of the elements
			 */
			ap = p->nptr[2];
			putRV( 0 , (ap->nl_block & 037), ap->value[0], 
				ap->extra_flags, p2type( ap ) );
			putop( PCC_MUL , PCCT_INT );
		    } else {
			if ( p -> range[ 0 ] != 0 ) {
			    putleaf( PCC_ICON , (int) p -> range[0] , 0 , PCCT_INT , (char *) 0 );
			    putop( PCC_MINUS , PCCT_INT );
			}
			    /*
			     *	multiply by the width of the elements
			     */
			if ( w != 1 ) {
			    putleaf( PCC_ICON , w , 0 , PCCT_INT , (char *) 0 );
			    putop( PCC_MUL , PCCT_INT );
			}
		    }
			/*
			 *	and add it to the base address
			 */
		    putop( PCC_PLUS , PCCM_ADDTYPE( p2type( np -> type ) , PCCTM_PTR ) );
		el = el->list_node.next;
#		endif PC
	}
	if (el != TR_NIL) {
	    if (np->type->class != ARRAY) {
		do {
			el = el->list_node.next;
			i++;
		} while (el != TR_NIL);
		error("Too many subscripts (%d given, %d required)", (char *) (i-1), (char *) d);
		return (-1);
	    } else {
		return(arycod(np->type, el, d));
	    }
	}
	return (d);
}

#ifdef OBJ
/*
 * Put out the conformant array bounds (lower bound, upper bound or width)
 * for conformant array type ctype.
 * The value of i determines which is being put
 * i = 0: lower bound, i=1: upper bound, i=2: width
 */
putcbnds(ctype, i)
struct nl *ctype;
int i;
{
	switch(width(ctype->type)) {
	    case 1:
		put(2, O_RV1 | (ctype->nl_block & 037) << 8+INDX,
			(int)ctype->nptr[i]->value[0]);
		break;
	    case 2:
		put(2, O_RV2 | (ctype->nl_block & 037) << 8+INDX,
			(int)ctype->nptr[i]->value[0]);
		break;
	    case 4:
	    default:
		put(2, O_RV4 | (ctype->nl_block & 037) << 8+INDX,
			(int)ctype->nptr[i]->value[0]);
	}
}
#endif OBJ
