/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: wide.c,v 2.3 84/07/19 12:01:37 guido Exp $";

/*
 * B editor -- Commands to make the focus larger and smaller in various ways.
 */

#include "b.h"
#include "bobj.h"
#include "node.h"
#include "supr.h"
#include "gram.h"


/*
 * Widen -- make the focus larger.
 */

Visible bool
widen(ep)
	register environ *ep;
{
	register node n;
	register int sym;
	register int ich;

	higher(ep);
	grow(ep);

	n = tree(ep->focus);
	sym = symbol(n);
	if (ep->mode == VHOLE && (ep->s1&1))
		ep->mode = FHOLE;
		
	switch (ep->mode) {

	case ATBEGIN:
	case ATEND:
		/* Shouldn't occur after grow(ep) */
		ep->mode = WHOLE;
		return Yes;

	case VHOLE:
		if (ep->s2 >= lenitem(ep))
			--ep->s2;
		ep->mode = SUBRANGE;
		ep->s3 = ep->s2;
		return Yes;

	case FHOLE:
		if (ep->s2 >= lenitem(ep)) {
			if (ep->s2 > 0)
				--ep->s2;
			else {
				leftvhole(ep);
				switch (ep->mode) {
				case ATBEGIN:
				case ATEND:
					ep->mode = WHOLE;
					return Yes;
				case VHOLE:
				case FHOLE:
					if (ep->s2 >= lenitem(ep)) {
						if (ep->s2 == 0) {
#ifndef NDEBUG
							debug("[Desperate in widen]");
#endif NDEBUG
							ep->mode = SUBSET;
							ep->s2 = ep->s1;
							return widen(ep);
						}
						--ep->s2;
					}
					ep->mode = SUBRANGE;
					ep->s3 = ep->s2;
					return Yes;
				}
				Abort();
			}
		}
		ep->mode = SUBRANGE;
		ep->s3 = ep->s2;
		return Yes;

	case SUBRANGE:
		ep->mode = SUBSET;
		ep->s2 = ep->s1;
		return Yes;
			
	case SUBSET:
		if (!issublist(sym) || width(lastchild(n)) == 0) {
			ep->mode = WHOLE;
			return Yes;
		}
		if (ep->s2 < 2*nchildren(n)) {
			ep->mode = SUBLIST;
			ep->s3 = 1;
			return Yes;
		}
		/* Fall through */
	case SUBLIST:
		for (;;) {
			ich = ichild(ep->focus);
			if (!up(&ep->focus)) {
				ep->mode = WHOLE;
				return Yes;
			}
			higher(ep);
			n = tree(ep->focus);
			if (ich != nchildren(n) || !samelevel(sym, symbol(n))) {
				ep->mode = SUBSET;
				ep->s1 = ep->s2 = 2*ich;
				return Yes;
			}
		}
		/* Not reached */
			
	case WHOLE:
		ich = ichild(ep->focus);
		if (!up(&ep->focus))
			return No;
		n = tree(ep->focus);
		if (issublist(symbol(n)) && ich < nchildren(n)) {
			ep->mode = SUBLIST;
			ep->s3 = 1;
		}
		return Yes;

	default:
		Abort();
		/* NOTREACHED */
	}
	/* Not reached */
}


/*
 * Narrow -- make the focus smaller.
 */

Visible bool
narrow(ep)
	register environ *ep;
{
	register node n;
	register int sym;
	register int nch;
	register string repr;
	
	higher(ep);

	shrink(ep);
	n = tree(ep->focus);
	sym = symbol(n);

	switch (ep->mode) {
		
	case ATBEGIN:
	case ATEND:
	case VHOLE:
	case FHOLE:
		return No;
	
	case SUBRANGE:
		if (ep->s3 > ep->s2)
			ep->s3 = ep->s2;
		else
			ep->mode = (ep->s1&1) ? FHOLE : VHOLE;
		return Yes;
		
	case SUBSET:
		if (ep->s1 <= 2) {
			nch = nchildren(n);	
			if (ep->s2 >= 2*nch && issublist(symbol(n))) {
				if (ep->s1 <= 1) {
					ep->s2 = 2*nch - 1;
					return Yes;
				}
				repr = noderepr(n)[0];
				if (!Fw_positive(repr)) {
					ep->s2 = 2*nch - 1;
					return Yes;
				}
			}
		}
		ep->s2 = ep->s1;
		return Yes;
		
	case SUBLIST:
		Assert(ep->s3 > 1);
		ep->s3 = 1;
		return Yes;
		
	case WHOLE:
		Assert(sym == Hole || sym == Optional);
		return No;
		
	default:
		Abort();
		/* NOTREACHED */
	}
}


Visible bool
extend(ep)
	register environ *ep;
{
	register node n;
	register int i;
	register int len;
	register int s1save;

	grow(ep);
	higher(ep);
	switch (ep->mode) {

	case VHOLE:
	case FHOLE:
	case ATBEGIN:
	case ATEND:
		return widen(ep);

	case SUBRANGE:
		len = lenitem(ep);
		if (ep->s3 < len-1)
			++ep->s3;
		else if (ep->s2 > 0)
			--ep->s2;
		else {
			ep->mode = SUBSET;
			ep->s2 = ep->s1;
			return extend(ep); /* Recursion! */
		}
		return Yes;

	case SUBSET:
		s1save = ep->s1;
		ep->s1 = ep->s2;
		if (nextnnitem(ep)) {
			ep->s2 = ep->s1;
			ep->s1 = s1save;
		}
		else {
			ep->s1 = s1save;
			prevnnitem(ep) || Abort();
		}
		return Yes;

	case WHOLE:
		return up(&ep->focus);

	case SUBLIST:
		n = tree(ep->focus);
		for (i = ep->s3; i > 1; --i)
			n = lastchild(n);
		if (samelevel(symbol(n), symbol(lastchild(n)))) {
			++ep->s3;
			return Yes;
		}
		ep->mode = WHOLE;
		if (symbol(lastchild(n)) != Optional)
			return Yes;
		return extend(ep); /* Recursion! */

	default:
		Abort();
		/* NOTREACHED */
	}
}


/*
 * Right-Narrow -- make the focus smaller, going to the last item of a list.
 */

Visible bool
rnarrow(ep)
	register environ *ep;
{
	register node n;
	register int i;
	register int sym;
	
	higher(ep);

	shrink(ep);
	n = tree(ep->focus);
	sym = symbol(n);
	if (sym == Optional || sym == Hole)
		return No;

	switch (ep->mode) {
		
	case ATBEGIN:
	case ATEND:
	case VHOLE:
	case FHOLE:
		return No;
	
	case SUBRANGE:
		if (ep->s3 > ep->s2)
			ep->s2 = ep->s3;
		else {
			++ep->s2;
			ep->mode = (ep->s1&1) ? FHOLE : VHOLE;
		}
		return Yes;
		
	case SUBSET:
		if (issublist(sym) && ep->s2 >= 2*nchildren(n)) {
			do {
				sym = symbol(n);
				s_downrite(ep);
				n = tree(ep->focus);
			} while (samelevel(sym, symbol(n))
				&& width(lastchild(n)) != 0);
			ep->mode = WHOLE;
			return Yes;
		}
		ep->s1 = ep->s2;
		return Yes;
		
	case SUBLIST:
		Assert(ep->s3 > 1);
		for (i = ep->s3; i > 1; --i)
			s_downi(ep, nchildren(tree(ep->focus)));
		ep->s3 = 1;
		return Yes;
		
	case WHOLE:
		Assert(sym == Hole || sym == Optional);
		return No;
		
	default:
		Abort();
		/* NOTREACHED */
	}
}
