/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)type.c	5.1 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "objfmt.h"
#include "tree_ty.h"

/*
 * Type declaration part
 */
/*ARGSUSED*/
typebeg( lineofytype , r )
    int	lineofytype;
{
    static bool	type_order = FALSE;
    static bool	type_seen = FALSE;

/*
 * this allows for multiple
 * declaration parts unless
 * standard option has been
 * specified.
 * If routine segment is being
 * compiled, do level one processing.
 */

#ifndef PI1
	if (!progseen)
		level1();
	line = lineofytype;
	if ( parts[ cbn ] & ( VPRT | RPRT ) ) {
	    if ( opt( 's' ) ) {
		standard();
		error("Type declarations should precede var and routine declarations");
	    } else {
		if ( !type_order ) {
		    type_order = TRUE;
		    warning();
		    error("Type declarations should precede var and routine declarations");
		}
	    }
	}
	if (parts[ cbn ] & TPRT) {
	    if ( opt( 's' ) ) {
		standard();
		error("All types should be declared in one type part");
	    } else {
		if ( !type_seen ) {
		    type_seen = TRUE;
		    warning();
		    error("All types should be declared in one type part");
		}
	    }
	}
	parts[ cbn ] |= TPRT;
#endif
	/*
	 * Forechain is the head of a list of types that
	 * might be self referential.  We chain them up and
	 * process them later.
	 */
	forechain = NIL;
#ifdef PI0
	send(REVTBEG);
#endif
}

type(tline, tid, tdecl)
	int tline;
	char *tid;
	register struct tnode *tdecl;
{
	register struct nl *np;
	struct nl *tnp;

	np = gtype(tdecl);
	line = tline;
	tnp = defnl(tid, TYPE, np, 0);
#ifndef PI0
	enter(tnp)->nl_flags |= (char) NMOD;
#else
	(void) enter(tnp);
	send(REVTYPE, tline, tid, tdecl);
#endif

#ifdef PC
	if (cbn == 1) {
	    stabgtype(tid, np, line);
	} else {
	    stabltype(tid, np);
	}
#endif PC

#	ifdef PTREE
	    {
		pPointer Type = TypeDecl( tid , tdecl );
		pPointer *Types;

		pSeize( PorFHeader[ nesting ] );
		Types = &( pDEF( PorFHeader[ nesting ] ).PorFTypes );
		*Types = ListAppend( *Types , Type );
		pRelease( PorFHeader[ nesting ] );
	    }
#	endif
}

typeend()
{

#ifdef PI0
	send(REVTEND);
#endif
	foredecl();
}

/*
 * Return a type pointer (into the namelist)
 * from a parse tree for a type, building
 * namelist entries as needed.
 */
struct nl *
gtype(r)
	register struct tnode *r;
{
	register struct nl *np;
	register int oline;
#ifdef OBJ
	long w;
#endif

	if (r == TR_NIL)
		return (NLNIL);
	oline = line;
	if (r->tag != T_ID)
		oline = line = r->lined.line_no;
	switch (r->tag) {
		default:
			panic("type");
		case T_TYID:
			r = (struct tnode *) (&(r->tyid_node.line_no));
		case T_ID:
			np = lookup(r->char_const.cptr);
			if (np == NLNIL)
				break;
			if (np->class != TYPE) {
#ifndef PI1
				error("%s is a %s, not a type as required", r->char_const.cptr, classes[np->class]);
#endif
				np = NLNIL;
				break;
			}
			np = np->type;
			break;
		case T_TYSCAL:
			np = tyscal(r);
			break;
		case T_TYCRANG:
			np = tycrang(r);
			break;
		case T_TYRANG:
			np = tyrang(r);
			break;
		case T_TYPTR:
			np = defnl((char *) 0, PTR, NLNIL, 0 );
			np -> ptr[0] = ((struct nl *) r->ptr_ty.id_node);
			np->nl_next = forechain;
			forechain = np;
			break;
		case T_TYPACK:
			np = gtype(r->comp_ty.type);
			break;
		case T_TYCARY:
		case T_TYARY:
			np = tyary(r);
			break;
		case T_TYREC:
			np = tyrec(r->comp_ty.type, 0);
#			ifdef PTREE
				/*
				 * mung T_TYREC[3] to point to the record
				 * for RecTCopy
				 */
			    r->comp_ty.nl_entry = np;
#			endif
			break;
		case T_TYFILE:
			np = gtype(r->comp_ty.type);
			if (np == NLNIL)
				break;
#ifndef PI1
			if (np->nl_flags & NFILES)
				error("Files cannot be members of files");
#endif
			np = defnl((char *) 0, FILET, np, 0);
			np->nl_flags |= NFILES;
			break;
		case T_TYSET:
			np = gtype(r->comp_ty.type);
			if (np == NLNIL)
				break;
			if (np->type == nl+TDOUBLE) {
#ifndef PI1
				error("Set of real is not allowed");
#endif
				np = NLNIL;
				break;
			}
			if (np->class != RANGE && np->class != SCAL) {
#ifndef PI1
				error("Set type must be range or scalar, not %s", nameof(np));
#endif
				np = NLNIL;
				break;
			}
#ifndef PI1
			if (width(np) > 2)
				error("Implementation restriction: sets must be indexed by 16 bit quantities");
#endif
			np = defnl((char *) 0, SET, np, 0);
			break;
	}
	line = oline;
#ifndef PC
	w = lwidth(np);
	if (w >= TOOMUCH) {
		error("Storage requirement of %s exceeds the implementation limit of %D by %D bytes",
			nameof(np), (char *) (long)(TOOMUCH-1), (char *) (long)(w-TOOMUCH+1));
		np = NLNIL;
	}
#endif
	return (np);
}

/*
 * Scalar (enumerated) types
 */
struct nl *
tyscal(r)
	struct tnode *r;	/* T_TYSCAL */
{
	register struct nl *np, *op, *zp;
	register struct tnode *v;
	int i;

	np = defnl((char *) 0, SCAL, NLNIL, 0);
	np->type = np;
	v = r->comp_ty.type;
	if (v == TR_NIL)
		return (NLNIL);
	i = -1;
	zp = np;
	for (; v != TR_NIL; v = v->list_node.next) {
		op = enter(defnl((char *) v->list_node.list, CONST, np, ++i));
#ifndef PI0
		op->nl_flags |= NMOD;
#endif
		op->value[1] = i;
		zp->chain = op;
		zp = op;
	}
	np->range[1] = i;
	return (np);
}

/*
 * Declare a subrange for conformant arrays.
 */
struct nl *
tycrang(r)
	register struct tnode *r;
{
	register struct nl *p, *op, *tp;

	tp = gtype(r->crang_ty.type);
	if ( tp == NLNIL )
		return (NLNIL);
	/*
	 * Just make a new type -- the lower and upper bounds must be
	 * set by params().
	 */
	p = defnl ( 0, CRANGE, tp, 0 );
	return(p);
}

/*
 * Declare a subrange.
 */
struct nl *
tyrang(r)
	register struct tnode *r;  /* T_TYRANG */
{
	register struct nl *lp, *hp;
	double high;
	int c, c1;

	gconst(r->rang_ty.const2);
	hp = con.ctype;
	high = con.crval;
	gconst(r->rang_ty.const1);
	lp = con.ctype;
	if (lp == NLNIL || hp == NLNIL)
		return (NLNIL);
	if (norange(lp) || norange(hp))
		return (NLNIL);
	c = classify(lp);
	c1 = classify(hp);
	if (c != c1) {
#ifndef PI1
		error("Can't mix %ss and %ss in subranges", nameof(lp), nameof(hp));
#endif
		return (NLNIL);
	}
	if (c == TSCAL && scalar(lp) != scalar(hp)) {
#ifndef PI1
		error("Scalar types must be identical in subranges");
#endif
		return (NLNIL);
	}
	if (con.crval > high) {
#ifndef PI1
		error("Range lower bound exceeds upper bound");
#endif
		return (NLNIL);
	}
	lp = defnl((char *) 0, RANGE, hp->type, 0);
	lp->range[0] = con.crval;
	lp->range[1] = high;
	return (lp);
}

norange(p)
	register struct nl *p;
{
	if (isa(p, "d")) {
#ifndef PI1
		error("Subrange of real is not allowed");
#endif
		return (1);
	}
	if (isnta(p, "bcsi")) {
#ifndef PI1
		error("Subrange bounds must be Boolean, character, integer or scalar, not %s", nameof(p));
#endif
		return (1);
	}
	return (0);
}

/*
 * Declare arrays and chain together the dimension specification
 */
struct nl *
tyary(r)
	struct tnode *r;
{
	struct nl *np;
	register struct tnode *tl, *s;
	register struct nl *tp, *ltp;
	int i, n;

	s = r;
	/* Count the dimensions */
	for (n = 0; s->tag == T_TYARY || s->tag == T_TYCARY;
					s = s->ary_ty.type, n++)
		/* NULL STATEMENT */;
	tp = gtype(s);
	if (tp == NLNIL)
		return (NLNIL);
	np = defnl((char *) 0, ARRAY, tp, 0);
	np->nl_flags |= (tp->nl_flags) & NFILES;
	ltp = np;
	i = 0;
	for (s = r; s->tag == T_TYARY || s->tag == T_TYCARY;
					s = s->ary_ty.type) {
	    for (tl = s->ary_ty.type_list; tl != TR_NIL; tl=tl->list_node.next){
		tp = gtype(tl->list_node.list);
		if (tp == NLNIL) {
			np = NLNIL;
			continue;
		}
		if ((tp->class == RANGE || tp->class == CRANGE) &&
		    tp->type == nl+TDOUBLE) {
#ifndef PI1
			error("Index type for arrays cannot be real");
#endif
			np = NLNIL;
			continue;
		}
		if (tp->class != RANGE && tp->class != SCAL && tp->class !=CRANGE){
#ifndef PI1
			error("Array index type is a %s, not a range or scalar as required", classes[tp->class]);
#endif
			np = NLNIL;
			continue;
		}
#ifndef PC
		if (tp->class == RANGE && bytes(tp->range[0], tp->range[1]) > 2) {
#ifndef PI1
			error("Value of dimension specifier too large or small for this implementation");
#endif
			continue;
		}
#endif
		if (tp->class != CRANGE)
			tp = nlcopy(tp);
		i++;
		ltp->chain = tp;
		ltp = tp;
	    }
	}
	if (np != NLNIL)
		np->value[0] = i;
	return (np);
}

/*
 * Delayed processing for pointers to
 * allow self-referential and mutually
 * recursive pointer constructs.
 */
foredecl()
{
	register struct nl *p;

	for (p = forechain; p != NLNIL; p = p->nl_next) {
		if (p->class == PTR && p -> ptr[0] != 0)
		{
			p->type = gtype((struct tnode *) p -> ptr[0]);
#			ifdef PTREE
			{
			    if ( pUSE( p -> inTree ).PtrTType == pNIL ) {
				pPointer	PtrTo = tCopy( p -> ptr[0] );

				pDEF( p -> inTree ).PtrTType = PtrTo;
			    }
			}
#			endif
#			ifdef PC
			    fixfwdtype(p);
#			endif
			p -> ptr[0] = 0;
		}
	}
}
