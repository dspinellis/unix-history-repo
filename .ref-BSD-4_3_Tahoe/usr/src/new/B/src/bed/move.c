/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: move.c,v 2.4 85/08/22 16:05:16 timo Exp $";

/*
 * B editor -- Process arrow keys in four directions, plus TAB.
 */

#include "b.h"
#include "bobj.h"
#include "node.h"
#include "supr.h"
#include "gram.h"

#define Left (-1)
#define Rite 1


/*
 * Common code for PREVIOUS and NEXT commands.
 */

Hidden bool
prevnext(ep, direction)
	environ *ep;
{
	node n;
	node n1;
	int nch;
	int i;
	int len;
	int sym;
	string *rp;

	higher(ep);
	switch (ep->mode) {
	case VHOLE:
	case FHOLE:
	case ATBEGIN:
	case ATEND:
		if (direction == Left)
			leftvhole(ep);
		else
			ritevhole(ep);
	}

	for (;;) {
		n = tree(ep->focus);
		nch = nchildren(n);
		rp = noderepr(n);

		switch (ep->mode) {

		case ATBEGIN:
		case ATEND:
			ep->mode = WHOLE;
			continue;

		case VHOLE:
		case FHOLE:
			if (direction == Rite) {
				if (ep->s1&1)
					len = Fwidth(rp[ep->s1/2]);
				else {
					n1 = child(n, ep->s1/2);
					len = width(n1);
				}
			}
			if (direction == Rite ? ep->s2 >= len : ep->s2 <= 0) {
				ep->mode = SUBSET;
				ep->s2 = ep->s1;
				return nextchar(ep, direction);
			}
			ep->s2 += direction;
			return Yes;

		case SUBRANGE:
			if (direction == Rite) {
				if (ep->s1&1)
					len = Fwidth(rp[ep->s1/2]);
				else {
					n1 = child(n, ep->s1/2);
					len = width(n1);
				}
			}
			if (direction == Left ? ep->s2 <= 0 : ep->s3 >= len-1) {
				ep->mode = SUBSET;
				ep->s2 = ep->s1;
				return nextchar(ep, direction);
			}
			if (direction == Rite)
				ep->s2 = ++ep->s3;
			else
				ep->s3 = --ep->s2;
			return Yes;

		case SUBSET:
			if (direction == Rite ? ep->s2 > 2*nch : ep->s1 <= 1) {
				ep->mode = WHOLE;
				continue;
			}
			if (direction == Rite)
				ep->s1 = ++ep->s2;
			else
				ep->s2 = --ep->s1;
			if (ep->s1&1) {
				if (!Fw_positive(rp[ep->s1/2]) || allspaces(rp[ep->s1/2]))
					continue;
			}
			else {
				sym = symbol(n);
				if (downi(&ep->focus, ep->s1/2)) {
					n = tree(ep->focus);
					if (((value)n)->type == Tex)
						s_up(ep);
					else {
						if (ep->s1 == 2*nch && direction == Rite
							&& issublist(sym) && samelevel(sym, symbol(n))) {
							ep->mode = SUBLIST;
							ep->s3 = 1;
							return Yes;
						}
						ep->mode = WHOLE;
						if (width(n) == 0)
							continue;
					}
				}
			}
			return Yes;

		case SUBLIST:
			sym = symbol(n);
			if (direction == Left) {
				i = ichild(ep->focus);
				if (!up(&ep->focus))
					return No;
				higher(ep);
				n = tree(ep->focus);
				if (i == nchildren(n) && samelevel(sym, symbol(n))) {
					ep->s3 = 1;
					return Yes;
				}
				ep->mode = SUBSET;
				ep->s1 = ep->s2 = 2*i;
				continue;
			}
			for (i = ep->s3; i > 0; --i)
				if (!downrite(&ep->focus))
					return No; /* Sorry... */
			if (samelevel(sym, symbol(tree(ep->focus))))
				ep->s3 = 1;
			else
				ep->mode = WHOLE;
			return Yes;

		case WHOLE:
			i = ichild(ep->focus);
			if (!up(&ep->focus))
				return No;
			higher(ep);
			ep->mode = SUBSET;
			ep->s1 = ep->s2 = 2*i;
			continue;

		default:
			Abort();
		}
	}
	/* Not reached */
}

Visible bool leftarrow(ep)
	environ *ep;
{
	int w;
	bool hole;

	if (narrow(ep)) {
		while (narrow(ep))
			;
		return Yes;
	}
	hole= ep->mode == WHOLE; /* Can't narrow and still WHOLE: */
				 /* a real hole which needs some hacking. */
	if (!previous(ep))
		return No;
	if (hole) {
		for (;;) {
			w= focwidth(ep);
			if (w >= 0 && w <= 1)
				break;
			if (!rnarrow(ep))
				return No;
		}
		narrow(ep);
	}
	else {
		while (rnarrow(ep))
			;
	}
	return Yes;
}

Visible bool ritearrow(ep)
	environ *ep;
{
	while (narrow(ep))
		;
	if (!next(ep))
		return No;
	while (narrow(ep))
		;
	return Yes;
}


Visible bool
previous(ep)
	environ *ep;
{
	if (!prevnext(ep, Left))
		return No;
	return Yes;
}


Visible bool
next(ep)
	environ *ep;
{
	if (!prevnext(ep, Rite))
		return No;
	return Yes;
}


/*
 * Position focus at next or previous char relative to current position.
 * Assume current position given as SUBSET.
 */

Hidden bool
nextchar(ep, direction)
	register environ *ep;
	register int direction;
{
	register int ich;
	register int nch;
	register node n;
	node n1;
	register int len;
	string *rp;

	Assert(ep->mode == SUBSET);
	for (;;) {
		n = tree(ep->focus);
		rp = noderepr(n);
		nch = nchildren(n);
		if (direction == Left)
			ep->s2 = --ep->s1;
		else
			ep->s1 = ++ep->s2;
		if (direction == Left ? ep->s1 < 1 : ep->s2 > 2*nch+1) {
			ich = ichild(ep->focus);
			if (!up(&ep->focus))
				return No; /* *ep is garbage now! */
			higher(ep);
			ep->s1 = ep->s2 = 2*ich;
			continue;
		}
		if (ep->s1&1) {
			len = Fwidth(rp[ep->s1/2]);
			if (len > 0) {
				ep->mode = SUBRANGE;
				ep->s2 = ep->s3 = direction == Left ? len-1 : 0;
				return Yes;
			}
			continue;
		}
		n1 = child(n, ep->s1/2);
		len = width(n1);
		if (len == 0)
			continue;
		if (!downi(&ep->focus, ep->s1/2))
			return No; /* Sorry... */
		n = tree(ep->focus);
		if (((value)n)->type == Tex) {
			s_up(ep);
			ep->mode = SUBRANGE;
			ep->s2 = ep->s3 = direction == Left ? len-1 : 0;
			return Yes;
		}
		if (direction == Left) {
			nch = nchildren(n);
			ep->s1 = ep->s2 = 2*(nch+1);
		}
		else
			ep->s1 = ep->s2 = 0;
	}
	/* Not reached */
}


/*
 * Up and down arrows.
 */

Hidden bool
updownarrow(ep, yincr)
	environ *ep;
	int yincr;
{
	int y, x;

	while (narrow(ep))
		;
	y= lineno(ep) + yincr;
	x= colno(ep);
	if (!gotoyx(ep, y, x))
		return No;
	gotofix(ep, y, x);
	while (narrow(ep))
		;
	return Yes;
}

Visible bool
uparrow(ep)
	environ *ep;
{
	return updownarrow(ep, -1);
}

Visible bool
downarrow(ep)
	environ *ep;
{
	return updownarrow(ep, 1);
}

Visible bool
upline(ep)
	register environ *ep;
{
	register int y;

	y = lineno(ep);
	if (y <= 0)
		return No;
	if (!gotoyx(ep, y-1, 0))
		return No;
	oneline(ep);
	return Yes;
}

Visible bool
downline(ep)
	register environ *ep;
{
	register int w;

	if (!parent(ep->focus) && ep->mode == ATEND)
		return No; /* Superfluous? */
	w = -focwidth(ep);
	if (w <= 0)
		w = 1;
	if (!gotoyx(ep, lineno(ep) + w, 0))
		return No;
	oneline(ep);
	return Yes;
}


/*
 * ACCEPT command
 * move to next Hole hole or to end of suggestion or to end of line.
 */


Visible bool
accept(ep)
	environ *ep;
{
	int i;
	string repr;

	shrink(ep);
	switch (ep->mode) {
	case ATBEGIN:
	case ATEND:
	case FHOLE:
	case VHOLE:
		ritevhole(ep);
	}
	if (symbol(tree(ep->focus)) == Hole)
		ep->mode = ATEND;
	switch (ep->mode) {
	case ATBEGIN:
	case SUBLIST:
	case WHOLE:
		i = 1;
		break;
	case ATEND:
		i = 2*nchildren(tree(ep->focus)) + 2;
		break;
	case SUBRANGE:
	case VHOLE:
	case FHOLE:
		i = ep->s1;
		if (ep->s2 > 0 && i > 2*nchildren(tree(ep->focus)))
			++i; /* Kludge so after E?LSE: the focus moves to ELSE: ? */
		break;
	case SUBSET:
		i = ep->s1 - 1;
		break;
	default:
		Abort();
	}
	ep->mode = WHOLE;
	for (;;) {
		if (i/2 == nchildren(tree(ep->focus))) {
			repr = noderepr(tree(ep->focus))[i/2];
			if (Fw_positive(repr))
				break;
		}
		if (tabstop(ep, i + 1))
			return Yes;
		i = 2*ichild(ep->focus) + 1;
		if (!up(&ep->focus))
			break;
		higher(ep);
	}
	ep->mode = ATEND;
	return Yes;
}


/*
 * Find suitable tab stops for accept.
 */
 
Hidden bool
tabstop(ep, i)
	environ *ep;
	int i;
{
	node n = tree(ep->focus);
	int nch;
	string repr;

	if (Type(n) == Tex)
		return No;
	nch = nchildren(n);
	if (i/2 > nch)
		return No;
	if (symbol(n) == Hole) {
		ep->mode = WHOLE;
		return Yes;
	}
	if (i < 2) {
		i = 2;
		if (width(n) < 0) {
			repr = noderepr(n)[0];
			if (Fw_negative(repr)) {
				ep->mode = ATBEGIN;
				leftvhole(ep);
				return Yes;
			}
		}
	}
	for (i /= 2; i <= nch; ++i) {
		s_downi(ep, i);
		if (tabstop(ep, 1))
			return Yes;
		s_up(ep);
	}
	return No;
}
