/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)pclval.c 1.4 6/1/81";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
	/*
	 *	and the rest of the file
	 */
#   include	"pc.h"
#   include	"pcops.h"

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
pclvalue( r , modflag , required )
	int	*r;
	int	modflag;
	int	required;
{
	register struct nl	*p;
	register		*c, *co;
	int			f, o;
	int			tr[2], trp[3];
	struct nl		*firstp;
	struct nl		*lastp;
	char			*firstsymbol;
	char			firstextra_flags;
	int			firstbn;

	if ( r == NIL ) {
		return NIL;
	}
	if ( nowexp( r ) ) {
		return NIL;
	}
	if ( r[0] != T_VAR ) {
		error("Variable required");	/* Pass mesgs down from pt of call ? */
		return NIL;
	}
	firstp = p = lookup( r[2] );
	if ( p == NIL ) {
		return NIL;
	}
	firstsymbol = p -> symbol;
	firstbn = bn;
	firstextra_flags = p -> extra_flags;
	c = r[3];
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
	if ( p -> class == WITHPTR ) {
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
	}
	    /*
	     *	this not only puts out the names of functions to call
	     *	but also does all the semantic checking of the qualifications.
	     */
	if ( ! nilfnil( p , c , modflag , firstp , r[2] ) ) {
	    return NIL;
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
			f = 1;		/* no lv on stack yet */
			o = p -> value[0];
			break;
		default:
			error("%s %s found where variable required", classes[p -> class], p -> symbol);
			return (NIL);
	}
	/*
	 * Loop and handle each
	 * qualification on the name
	 */
	if ( c == NIL &&
	    ( modflag & ASGN ) &&
	    ( p -> value[ NL_FORV ] & FORVAR ) ) {
		error("Can't modify the for variable %s in the range of the loop", p -> symbol);
		return (NIL);
	}
	for ( ; c != NIL ; c = c[2] ) {
		co = c[1];
		if ( co == NIL ) {
			return NIL;
		}
		lastp = p;
		p = p -> type;
		if ( p == NIL ) {
			return NIL;
		}
		switch ( co[0] ) {
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
					    putleaf( P2ICON , o , 0 , P2INT
						    , 0 );
					    putop( P2PLUS , P2PTR | P2CHAR );
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
				    putop( P2UNARY P2MUL , p2type( p ) );
				    if ( opt( 't' ) ) {
					putop( P2CALL , P2INT );
				    }
				} else {
				    putop( P2CALL , P2INT );
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
					    putleaf( P2ICON , o , 0 , P2INT
						    , 0 );
					    putop( P2PLUS , P2INT );
					}
				}
				arycod( p , co[1] );
				f = o = 0;
				continue;
			case T_FIELD:
				/*
				 * Field names are just
				 * an offset with some 
				 * semantic checking.
				 */
				p = reclook(p, co[1]);
				o += p -> value[0];
				continue;
			default:
				panic("lval2");
		}
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
		    putleaf( P2ICON , o , 0 , P2INT , 0 );
		    putop( P2PLUS , P2INT );
		}
		if ( required == RREQ ) {
		    putop( P2UNARY P2MUL , p2type( p -> type ) );
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
nilfnil( p , c , modflag , firstp , r2 )
    struct nl	*p;
    int		*c;
    int		modflag;
    struct nl	*firstp;
    char	*r2;		/* no, not r2-d2 */
    {
	int		*co;
	struct nl	*lastp;
	int		t;

	if ( c == NIL ) {
	    return TRUE;
	}
	co = (int *) ( c[1] );
	if ( co == NIL ) {
		return FALSE;
	}
	lastp = p;
	p = p -> type;
	if ( p == NIL ) {
		return FALSE;
	}
	switch ( co[0] ) {
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
		    t = arycod( p , co[1] );
		    codeon();
		    switch ( t ) {
			    case 0:
				    return FALSE;
			    case -1:
				    goto bad;
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
		    if ( co[1] == NIL ) {
			    return FALSE;
		    }
		    p = reclook( p , co[1] );
		    if ( p == NIL ) {
			    error("%s is not a field in this record", co[1]);
			    goto bad;
		    }
		    if ( modflag & MOD ) {
			    p -> nl_flags |= NMOD;
		    }
		    if ( ( modflag & NOUSE ) == 0 || lptr( c[2] ) ) {
			    p -> nl_flags |= NUSED;
		    }
		    break;
	    default:
		    panic("nilfnil");
	}
	    /*
	     *	recursive call, check the rest of the qualifications.
	     */
	if ( ! nilfnil( p , c[2] , modflag , firstp , r2 ) ) {
	    return FALSE;
	}
	    /*
	     *	the point of all this.
	     */
	if ( co[0] == T_PTR ) {
	    if ( p -> class == PTR ) {
		    if ( opt( 't' ) ) {
			putleaf( P2ICON , 0 , 0
			    , ADDTYPE( P2FTN | P2INT , P2PTR )
			    , "_NIL" );
		    }
	    } else {
		    putleaf( P2ICON , 0 , 0
			, ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_FNIL" );
	    }
	}
	return TRUE;
bad:
	cerror("Error occurred on qualification of %s", r2);
	return FALSE;
    }
#endif PC
