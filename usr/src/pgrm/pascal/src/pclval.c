/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pclval.c	5.1 (Berkeley) 6/5/85";
#endif not lint


#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "tree_ty.h"
#ifdef PC
	/*
	 *	and the rest of the file
	 */
#   include	"pc.h"
#   include	<pcc.h>

extern	int flagwas;
/*
 * pclvalue computes the address
 * of a qualified name and
 * leaves it on the stack.
 * for pc, it can be asked for either an lvalue or an rvalue.
 * the semantics are the same, only the code is different.
 * for putting out calls to check for nil and fnil,
 * we have to traverse the list of qualifications twice:
 * once to put out the calls and once to put out the address to be checked.
 */
struct nl *
pclvalue( var , modflag , required )
	struct tnode	*var;
	int	modflag;
	int	required;
{
	register struct nl	*p;
	register struct tnode 	*c, *co;
	int			f, o;
	struct tnode		l_node, tr;
	VAR_NODE		*v_node;
	LIST_NODE		*tr_ptr;
	struct nl		*firstp, *lastp;
	char			*firstsymbol;
	char			firstextra_flags;
	int			firstbn;
	int			s;

	if ( var == TR_NIL ) {
		return NLNIL;
	}
	if ( nowexp( var ) ) {
		return NLNIL;
	}
	if ( var->tag != T_VAR ) {
		error("Variable required");	/* Pass mesgs down from pt of call ? */
		return NLNIL;
	}
	v_node = &(var->var_node);
	firstp = p = lookup( v_node->cptr );
	if ( p == NLNIL ) {
		return NLNIL;
	}
	firstsymbol = p -> symbol;
	firstbn = bn;
	firstextra_flags = p -> extra_flags;
	c = v_node->qual;
	if ( ( modflag & NOUSE ) && ! lptr( c ) ) {
		p -> nl_flags = flagwas;
	}
	if ( modflag & MOD ) {
		p -> nl_flags |= NMOD;
	}
	/*
	 * Only possibilities for p -> class here
	 * are the named classes, i.e. CONST, TYPE
	 * VAR, PROC, FUNC, REF, or a WITHPTR.
	 */
	 tr_ptr = &(l_node.list_node);
	if ( p -> class == WITHPTR ) {
		/*
		 * Construct the tree implied by
		 * the with statement
		 */
	    l_node.tag = T_LISTPP;
	    tr_ptr->list = &(tr);
	    tr_ptr->next = v_node->qual;
	    tr.tag = T_FIELD;
	    tr.field_node.id_ptr = v_node->cptr;
	    c = &(l_node);
	}
	    /*
	     *	this not only puts out the names of functions to call
	     *	but also does all the semantic checking of the qualifications.
	     */
	if ( ! nilfnil( p , c , modflag , firstp , v_node->cptr ) ) {
	    return NLNIL;
	}
	switch (p -> class) {
		case WITHPTR:
		case REF:
			/*
			 * Obtain the indirect word
			 * of the WITHPTR or REF
			 * as the base of our lvalue
			 */
			putRV( firstsymbol , firstbn , p -> value[ 0 ] ,
				firstextra_flags , p2type( p ) );
			firstsymbol = 0;
			f = 0;		/* have an lv on stack */
			o = 0;
			break;
		case VAR:
			if (p->type->class != CRANGE) {
				f = 1;		/* no lv on stack yet */
				o = p -> value[0];
			} else {
				error("Conformant array bound %s found where variable required", p->symbol);
				return(NIL);
			}
			break;
		default:
			error("%s %s found where variable required", classes[p -> class], p -> symbol);
			return (NLNIL);
	}
	/*
	 * Loop and handle each
	 * qualification on the name
	 */
	if ( c == NIL &&
	    ( modflag & ASGN ) &&
	    ( p -> value[ NL_FORV ] & FORVAR ) ) {
		error("Can't modify the for variable %s in the range of the loop", p -> symbol);
		return (NLNIL);
	}
	s = 0;
	for ( ; c != TR_NIL ; c = c->list_node.next ) {
		co = c->list_node.list;
		if ( co == TR_NIL ) {
			return NLNIL;
		}
		lastp = p;
		p = p -> type;
		if ( p == NLNIL ) {
			return NLNIL;
		}
		/*
		 * If we haven't seen enough subscripts, and the next
		 * qualification isn't array reference, then it's an error.
		 */
		if (s && co->tag != T_ARY) {
			error("Too few subscripts (%d given, %d required)",
				s, p->value[0]);
		}
		switch ( co->tag ) {
			case T_PTR:
				/*
				 * Pointer qualification.
				 */
				if ( f ) {
					putLV( firstsymbol , firstbn , o ,
					    firstextra_flags , p2type( p ) );
					firstsymbol = 0;
				} else {
					if (o) {
					    putleaf( PCC_ICON , o , 0 , PCCT_INT
						    , (char *) 0 );
					    putop( PCC_PLUS , PCCTM_PTR | PCCT_CHAR );
					}
				}
				    /*
				     * Pointer cannot be
				     * nil and file cannot
				     * be at end-of-file.
				     * the appropriate function name is 
				     * already out there from nilfnil.
				     */
				if ( p -> class == PTR ) {
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
					 */
				    putop( PCCOM_UNARY PCC_MUL , p2type( p ) );
				    if ( opt( 't' ) ) {
					putop( PCC_CALL , PCCT_INT );
				    }
				} else {
				    putop( PCC_CALL , PCCT_INT );
				}
				f = o = 0;
				continue;
			case T_ARGL:
			case T_ARY:
				if ( f ) {
					putLV( firstsymbol , firstbn , o ,
					    firstextra_flags , p2type( p ) );
					firstsymbol = 0;
				} else {
					if (o) {
					    putleaf( PCC_ICON , o , 0 , PCCT_INT
						    , (char *) 0 );
					    putop( PCC_PLUS , PCCT_INT );
					}
				}
				s = arycod( p , co->ary_node.expr_list, s);
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
				p = reclook(p, co->field_node.id_ptr);
				o += p -> value[0];
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
		if ( required == LREQ ) {
		    putLV( firstsymbol , firstbn , o ,
			    firstextra_flags , p2type( p -> type ) );
		} else {
		    putRV( firstsymbol , firstbn , o ,
			    firstextra_flags , p2type( p -> type ) );
		}
	} else {
		if (o) {
		    putleaf( PCC_ICON , o , 0 , PCCT_INT , (char *) 0 );
		    putop( PCC_PLUS , PCCT_INT );
		}
		if ( required == RREQ ) {
		    putop( PCCOM_UNARY PCC_MUL , p2type( p -> type ) );
		}
	}
	return ( p -> type );
}

    /*
     *	this recursively follows done a list of qualifications
     *	and puts out the beginnings of calls to fnil for files
     *	or nil for pointers (if checking is on) on the way back.
     *	this returns true or false.
     */
bool
nilfnil( p , c , modflag , firstp , r2 )
    struct nl	 *p;
    struct tnode *c;
    int		modflag;
    struct nl	*firstp;
    char	*r2;		/* no, not r2-d2 */
    {
	struct tnode 	*co;
	struct nl	*lastp;
	int		t;
	static int	s = 0;

	if ( c == TR_NIL ) {
	    return TRUE;
	}
	co = ( c->list_node.list );
	if ( co == TR_NIL ) {
		return FALSE;
	}
	lastp = p;
	p = p -> type;
	if ( p == NLNIL ) {
		return FALSE;
	}
	switch ( co->tag ) {
	    case T_PTR:
		    /*
		     * Pointer qualification.
		     */
		    lastp -> nl_flags |= NUSED;
		    if ( p -> class != PTR && p -> class != FILET) {
			    error("^ allowed only on files and pointers, not on %ss", nameof(p));
			    goto bad;
		    }
		    break;
	    case T_ARGL:
		    if ( p -> class != ARRAY ) {
			    if ( lastp == firstp ) {
				    error("%s is a %s, not a function", r2, classes[firstp -> class]);
			    } else {
				    error("Illegal function qualificiation");
			    }
			    return FALSE;
		    }
		    recovered();
		    error("Pascal uses [] for subscripting, not ()");
		    /* and fall through */
	    case T_ARY:
		    if ( p -> class != ARRAY ) {
			    error("Subscripting allowed only on arrays, not on %ss", nameof(p));
			    goto bad;
		    }
		    codeoff();
		    s = arycod( p , co->ary_node.expr_list , s );
		    codeon();
		    switch ( s ) {
			    case 0:
				    return FALSE;
			    case -1:
				    goto bad;
		    }
		    if (s == p->value[0]) {
			    s = 0;
		    } else {
			    p = lastp;
		    }
		    break;
	    case T_FIELD:
		    /*
		     * Field names are just
		     * an offset with some 
		     * semantic checking.
		     */
		    if ( p -> class != RECORD ) {
			    error(". allowed only on records, not on %ss", nameof(p));
			    goto bad;
		    }
		    if ( co->field_node.id_ptr == NIL ) {
			    return FALSE;
		    }
		    p = reclook( p , co->field_node.id_ptr );
		    if ( p == NIL ) {
			    error("%s is not a field in this record", co->field_node.id_ptr);
			    goto bad;
		    }
		    if ( modflag & MOD ) {
			    p -> nl_flags |= NMOD;
		    }
		    if ((modflag & NOUSE) == 0 || lptr(c->field_node.other )) {
			    p -> nl_flags |= NUSED;
		    }
		    break;
	    default:
		    panic("nilfnil");
	}
	    /*
	     *	recursive call, check the rest of the qualifications.
	     */
	if ( ! nilfnil( p , c->list_node.next , modflag , firstp , r2 ) ) {
	    return FALSE;
	}
	    /*
	     *	the point of all this.
	     */
	if ( co->tag == T_PTR ) {
	    if ( p -> class == PTR ) {
		    if ( opt( 't' ) ) {
			putleaf( PCC_ICON , 0 , 0
			    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			    , "_NIL" );
		    }
	    } else {
		    putleaf( PCC_ICON , 0 , 0
			, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_FNIL" );
	    }
	}
	return TRUE;
bad:
	cerror("Error occurred on qualification of %s", r2);
	return FALSE;
    }
#endif PC
