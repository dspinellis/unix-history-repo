/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cset.c	5.1 (Berkeley) 6/5/85";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "tree_ty.h"
#ifdef PC
#include "pc.h"
#include <pcc.h>
#include "align.h"
#endif PC

/*
 * CONSETS causes compile time constant sets to be constructed here.
 *
 * COMPSETSZE defines the maximum number of longs to be used in
 *	constant set construction
 */
#define CONSETS
#define COMPSETSZE 10

#define BITSPERBYTE 8
#define BITSPERLONG 32
#define LG2BITSBYTE 3
#define MSKBITSBYTE 0x07
#define LG2BITSLONG 5
#define MSKBITSLONG 0x1f

/*
 *	rummage through a `constant' set (i.e. anything within [ ]'s) tree
 *	and decide if this is a compile time constant set or a runtime set.
 *	this information is returned in a structure passed from the caller.
 *	while rummaging, this also reorders the tree so that all ranges
 *	preceed all singletons.
 */
bool
precset( r , settype , csetp )
	struct tnode	*r;
	struct nl	*settype;
	struct csetstr	*csetp;
{
	register struct tnode	*e;
	register struct nl	*t;
	register struct nl	*exptype;
	register struct tnode	*el;
	register struct tnode	*pairp;
	register struct tnode	*singp;
	struct tnode		*ip;
	int			lower;
	int			upper;
	bool			setofint;

	csetp -> csettype = NIL;
	csetp -> paircnt = 0;
	csetp -> singcnt = 0;
	csetp -> comptime = TRUE;
	setofint = FALSE;
	if ( settype != NIL ) {
	    if ( settype -> class == SET ) {
		    /*
		     *	the easy case, we are told the type of the set.
		     */
		exptype = settype -> type;
	    } else {
		    /*
		     *	we are told the type, but it's not a set
		     *	supposedly possible if someone tries
		     *	e.g string context [1,2] = 'abc'
		     */
		error("Constant set involved in non set context");
		return csetp -> comptime;
	    }
	} else {
		/*
		 * So far we have no indication
		 * of what the set type should be.
		 * We "look ahead" and try to infer
		 * The type of the constant set
		 * by evaluating one of its members.
		 */
	    e = r->cset_node.el_list;
	    if (e == NIL) {
		    /*
		     *	tentative for [], return type of `intset'
		     */
		settype = lookup( (char *) intset );
		if ( settype == NIL ) {
		    panic( "empty set" );
		}
		settype = settype -> type;
		if ( settype == NIL ) {
		    return csetp -> comptime;
		}
		if ( isnta( settype , "t" ) ) {
		    error("Set default type \"intset\" is not a set");
		    return csetp -> comptime;
		}
		csetp -> csettype = settype;
		setran( settype -> type );
		if (((set.uprbp + 1) >> LG2BITSLONG) >= COMPSETSZE)
			csetp -> comptime = FALSE;
		return csetp -> comptime;
	    }
	    e = e->list_node.list;
	    if (e == NIL) {
		return csetp -> comptime;
	    }
	    if (e->tag == T_RANG) {
		    e = e->rang.expr1;
	    }
	    codeoff();
	    t = rvalue(e, NLNIL , RREQ );
	    codeon();
	    if (t == NIL) {
		return csetp -> comptime;
	    }
		/*
		 * The type of the set, settype, is
		 * deemed to be a set of the base type
		 * of t, which we call exptype.  If,
		 * however, this would involve a
		 * "set of integer", we cop out
		 * and use "intset"'s current scoped
		 * type instead.
		 */
	    if (isa(t, "r")) {
		    error("Sets may not have 'real' elements");
		    return csetp -> comptime;
	    }
	    if (isnta(t, "bcsi")) {
		    error("Set elements must be scalars, not %ss", nameof(t));
		    return csetp -> comptime;
	    }
	    if (isa(t, "i")) {
		    settype = lookup((char *) intset);
		    if (settype == NIL)
			    panic("intset");
		    settype = settype->type;
		    if (settype == NIL)
			    return csetp -> comptime;
		    if (isnta(settype, "t")) {
			    error("Set default type \"intset\" is not a set");
			    return csetp -> comptime;
		    }
		    exptype = settype->type;
			/*
			 *	say we are doing an intset
			 *	but, if we get out of range errors for intset
			 *	we punt constructing the set at	compile time.
			 */
		    setofint = TRUE;
	    } else {
			exptype = t->type;
			if (exptype == NIL)
				return csetp -> comptime;
			if (exptype->class != RANGE)
				exptype = exptype->type;
			settype = defnl((char *) 0, SET, exptype, 0);
	    }
	}
	csetp -> csettype = settype;
#	ifndef CONSETS
	    csetp -> comptime = FALSE;
#	endif CONSETS
	setran( exptype );
	if (((set.uprbp + 1) >> LG2BITSLONG) >= COMPSETSZE)
		csetp -> comptime = FALSE;
	lower = set.lwrb;
	upper = set.lwrb + set.uprbp;
	pairp = NIL;
	singp = NIL;
	codeoff();
	while ( el = r->cset_node.el_list ) {
		e = el->list_node.list;
		if (e == NIL) {
			    /*
			     *	don't hang this one anywhere.
			     */
			csetp -> csettype = NIL;
			r->cset_node.el_list = el->list_node.next;
			continue;
		}
		if (e->tag == T_RANG) {
			if ( csetp -> comptime && constval( e->rang.expr2 ) ) {
#ifdef CONSETS
			    t = con.ctype;
			    if ( con.crval < lower || con.crval > upper ) {
				if ( setofint ) {
				    csetp -> comptime = FALSE;
				} else {
				    error("Range upper bound of %D out of set bounds" , ((long)con.crval) );
				    csetp -> csettype = NIL;
				}
			    }
#endif CONSETS
			} else {
			    csetp -> comptime = FALSE;
			    t = rvalue(e->rang.expr2, NLNIL , RREQ );
			    if (t == NIL) {
				    (void) rvalue(e->rang.expr1, NLNIL , RREQ );
				    goto pairhang;
			    }
			}
			if (incompat(t, exptype, e->rang.expr2)) {
				cerror("Upper bound of element type clashed with set type in constant set");
			}
			if ( csetp -> comptime && constval( e->rang.expr1 ) ) {
#ifdef CONSETS
			    t = con.ctype;
			    if ( con.crval < lower || con.crval > upper ) {
				if ( setofint ) {
				    csetp -> comptime = FALSE;
				} else {
				    error("Range lower bound of %D out of set bounds" , ((long)con.crval) );
				    csetp -> csettype = NIL;
				}
			    }
#endif CONSETS
			} else {
			    csetp -> comptime = FALSE;
			    t = rvalue(e->rang.expr1, NLNIL , RREQ );
			    if (t == NIL) {
				    goto pairhang;
			    }
			}
			if (incompat(t, exptype, e->rang.expr1)) {
				cerror("Lower bound of element type clashed with set type in constant set");
			}
pairhang:
			    /*
			     *	remove this range from the tree list and 
			     *	hang it on the pairs list.
			     */
			ip = el->list_node.next;
			el->list_node.next = pairp;
			pairp = r->cset_node.el_list;
			r->cset_node.el_list = ip;
			csetp -> paircnt++;
		} else {
			if ( csetp -> comptime && constval( e ) ) {
#ifdef CONSETS
			    t = con.ctype;
			    if ( con.crval < lower || con.crval > upper ) {
				if ( setofint ) {
				    csetp -> comptime = FALSE;
				} else {
				    error("Value of %D out of set bounds" , ((long)con.crval) );
				    csetp -> csettype = NIL;
				}
			    }
#endif CONSETS
			} else {
			    csetp -> comptime = FALSE;
			    t = rvalue( e, NLNIL , RREQ );
			    if (t == NIL) {
				    goto singhang;
			    }
			}
			if (incompat(t, exptype, e)) {
				cerror("Element type clashed with set type in constant set");
			}
singhang:
			    /*
			     *	take this expression off the tree list and
			     *	hang it on the list of singletons.
			     */
			ip = el->list_node.next;
			el->list_node.next = singp;
			singp = r->cset_node.el_list;
			r->cset_node.el_list = ip;
			csetp -> singcnt++;
		}
	}
	codeon();
#	ifdef PC
	    if ( pairp != NIL ) {
		for ( el = pairp ; el->list_node.next != NIL ; el = el->list_node.next ) /* void */;
		el->list_node.next = singp;
		r->cset_node.el_list = pairp;
	    } else {
		r->cset_node.el_list = singp;
	    }
#	endif PC
#	ifdef OBJ
	    if ( singp != NIL ) {
		for ( el = singp ; el->list_node.next != NIL ; el = el->list_node.next ) /* void */;
		el->list_node.next = pairp;
		r->cset_node.el_list = singp;
	    } else {
		r->cset_node.el_list = pairp;
	    }
#	endif OBJ
	if ( csetp -> csettype == NIL ) {
	    csetp -> comptime = TRUE;
	}
	return csetp -> comptime;
}

#ifdef CONSETS
    /*
     *	mask[i] has the low i bits turned off.
     */
long	mask[] = {	
#		ifdef DEC11
		    0xffffffff , 0xfffffffe , 0xfffffffc , 0xfffffff8 ,
		    0xfffffff0 , 0xffffffe0 , 0xffffffc0 , 0xffffff80 ,
		    0xffffff00 , 0xfffffe00 , 0xfffffc00 , 0xfffff800 ,
		    0xfffff000 , 0xffffe000 , 0xffffc000 , 0xffff8000 ,
		    0xffff0000 , 0xfffe0000 , 0xfffc0000 , 0xfff80000 ,
		    0xfff00000 , 0xffe00000 , 0xffc00000 , 0xff800000 ,
		    0xff000000 , 0xfe000000 , 0xfc000000 , 0xf8000000 ,
		    0xf0000000 , 0xe0000000 , 0xc0000000 , 0x80000000 ,
		    0x00000000
#		else
		    0xffffffff , 0xfeffffff , 0xfcffffff , 0xf8ffffff ,
		    0xf0ffffff , 0xe0ffffff , 0xc0ffffff , 0x80ffffff ,
		    0x00ffffff , 0x00feffff , 0x00fcffff , 0x00f8ffff ,
		    0x00f0ffff , 0x00e0ffff , 0x00c0ffff , 0x0080ffff ,
		    0x0000ffff , 0x0000feff , 0x0000fcff , 0x0000f8ff ,
		    0x0000f0ff , 0x0000e0ff , 0x0000c0ff , 0x000080ff ,
		    0x000000ff , 0x000000fe , 0x000000fc , 0x000000f8 ,
		    0x000000f0 , 0x000000e0 , 0x000000c0 , 0x00000080 ,
		    0x00000000
#		endif DEC11
	    };
    /*
     *	given a csetstr, either
     *	    put out a compile time constant set and an lvalue to it.
     *	or
     *	    put out rvalues for the singletons and the pairs
     *	    and counts of each.
     */
#endif CONSETS
postcset( r , csetp )
    struct tnode	*r;
    struct csetstr	*csetp;
    {
	register struct tnode	*el;
	register struct tnode	*e;
	int		lower;
	int		upper;
	int		lowerdiv;
	int		lowermod;
	int		upperdiv;
	int		uppermod;
	long		*lp;
	long		*limit;
	long		tempset[ COMPSETSZE ];
	long		temp;
	char		*cp;
#	ifdef PC
	    int		label;
	    char	labelname[ BUFSIZ ];
#	endif PC

	if ( csetp -> comptime ) {
#ifdef CONSETS
	    setran( ( csetp -> csettype ) -> type );
	    limit = &tempset[ ( set.uprbp >> LG2BITSLONG ) + 1 ];
	    for ( lp = &tempset[0] ; lp < limit ; lp++ ) {
		*lp = 0;
	    }
	    for ( el = r->cset_node.el_list ; el != NIL ; el = el->list_node.next ) {
		e = el->list_node.list;
		if ( e->tag == T_RANG ) {
		    (void) constval( e->rang.expr1 );
		    lower = con.crval;
		    (void) constval( e->rang.expr2 );
		    upper = con.crval;
		    if ( upper < lower ) {
			continue;
		    }
		    lowerdiv = ( lower - set.lwrb ) >> LG2BITSLONG;
		    lowermod = ( lower - set.lwrb ) & MSKBITSLONG;
		    upperdiv = ( upper - set.lwrb ) >> LG2BITSLONG;
		    uppermod = ( upper - set.lwrb ) & MSKBITSLONG;
		    temp = mask[ lowermod ];
		    if ( lowerdiv == upperdiv ) {
			temp &= ~mask[ uppermod + 1 ];
		    }
		    tempset[ lowerdiv ] |= temp;
		    limit = &tempset[ upperdiv-1 ];
		    for ( lp = &tempset[ lowerdiv+1 ] ; lp <= limit ; lp++ ) {
			*lp |= 0xffffffff;
		    }
		    if ( lowerdiv != upperdiv ) {
			tempset[ upperdiv ] |= ~mask[ uppermod + 1 ];
		    }
		} else {
		    (void) constval( e );
		    temp = con.crval - set.lwrb;
		    cp = (char *)tempset;
		    cp[temp >> LG2BITSBYTE] |= (1 << (temp & MSKBITSBYTE));
		}
	    }
	    if ( !CGENNING )
		return;
#	    ifdef PC
		label = (int) getlab();
		putprintf("	.data" , 0 );
		aligndot(A_SET);
		(void) putlab( (char *) label );
		lp = &( tempset[0] );
		limit = &tempset[ ( set.uprbp >> LG2BITSLONG ) + 1 ];
		while (lp < limit) {
		    putprintf("	.long	0x%x", 1, (int) (*lp++));
		    for (temp = 2 ; temp <= 8 && lp < limit ; temp++) {
			putprintf(",0x%x", 1, (int) (*lp++));
		    }
		    putprintf("", 0);
		}
		putprintf("	.text", 0);
		sprintf( labelname , PREFIXFORMAT , LABELPREFIX , (char *) label );
		putleaf( PCC_ICON , 0 , 0 , PCCTM_PTR | PCCT_STRTY , labelname );
#	    endif PC
#	    ifdef OBJ
		(void) put(2, O_CON, (int)(((set.uprbp >> LG2BITSLONG) + 1) *
				 (BITSPERLONG >> LG2BITSBYTE)));
		lp = &( tempset[0] );
		limit = &tempset[ ( set.uprbp >> LG2BITSLONG ) + 1 ];
		while ( lp < limit ) {
		    (void) put(2, O_CASE4, (int) (*lp ++));
		}
#	    endif OBJ
#else
		panic("const cset");
#endif CONSETS
	} else {
#	    ifdef PC
		putleaf( PCC_ICON , (int) csetp -> paircnt , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putleaf( PCC_ICON , (int) csetp -> singcnt , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		for ( el = r->cset_node.el_list ; el != NIL ; el = el->list_node.next ) {
		    e = el->list_node.list;
		    if ( e->tag == T_RANG ) {
			(void) rvalue( e->rang.expr2 , NLNIL , RREQ );
			putop( PCC_CM , PCCT_INT );
			(void) rvalue( e->rang.expr1 , NLNIL , RREQ );
			putop( PCC_CM , PCCT_INT );
		    } else {
			(void) rvalue( e , NLNIL , RREQ );
			putop( PCC_CM , PCCT_INT );
		    }
		}
#	    endif PC
#	    ifdef OBJ
		for ( el = r->cset_node.el_list ; el != NIL ; el = el->list_node.next ) {
		    e = el->list_node.list;
		    if ( e->tag == T_RANG ) {
			(void) stkrval( e->rang.expr1 , NLNIL , (long) RREQ );
			(void) stkrval( e->rang.expr2 , NLNIL , (long) RREQ );
		    } else {
			(void) stkrval( e , NLNIL , (long) RREQ );
		    }
		}
		(void) put(2 , O_CON24 , (int)csetp -> singcnt );
		(void) put(2 , O_CON24 , (int)csetp -> paircnt );
#	    endif OBJ
	}
}
