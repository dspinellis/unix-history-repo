/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: line.c,v 2.4 85/08/22 16:04:53 timo Exp $";

/*
 * B editor -- Routines for treating the parse tree as a sequence of lines.
 *
 * WARNING: The routines in this file (and many others!) assume that a
 * `newline' can only occur in the zero'th representation string of a node
 * (i.e., rp[0]).
 */

#include "b.h"
#include "bobj.h"
#include "node.h"
#include "gram.h"
#include "supr.h"


/*
 * Compute equality of subtrees, based on common descent.
 * Strings are not checked for characterwise equality, but must
 * be the same pointer; other nodes must have the same symbol and
 * their children must be equal in this sense (equal pointers are
 * always used as a shortcut).
 *
 * (Used by screen update algorithm only.)
 */

Visible bool
eqlines(n1, n2)
	node n1;
	node n2;
{
	register node nn1;
	register node nn2;
	register int w1;
	register int w2;
	register int nch;
	register int i;

	if (n1 == n2)
		return Yes;
	if (Type(n1) != Nod || Type(n2) != Nod)
		return No;
	if (symbol(n1) != symbol(n2))
		return No;
	nch = nchildren(n1);
	Assert(nch == nchildren(n2));
	for (i = 1; i <= nch; ++i) {
		nn1 = child(n1, i);
		nn2 = child(n2, i);
		w1 = width(nn1);
		w2 = width(nn2);
		if (w1 >= 0 && w2 >= 0) {
			if (!eqlines(nn1, nn2))
				return No;
		}
		else {
			if (nn1 == nn2)
				return Yes;
			if (fwidth(noderepr(nn1)[0]) < 0 || fwidth(noderepr(nn2)[0]) < 0)
				return linelen(n1) == linelen(n2);
			return eqlines(nn1, nn2);
		}
	}
	return Yes;
}


/*
 * Compute the length of the line beginning at the current node.
 */
 
Visible int
linelen(n)
	node n;
{
	register node nn;
	register string *rp = noderepr(n);
	register int w;
	register int nch = nchildren(n);
	register int i;
	register int len = fwidth(rp[0]);

	if (len < 0)
		len = 0;
	for (i = 1; i <= nch; ++i) {
		nn = child(n, i);
		w = width(nn);
		if (w >= 0)
			len += w;
		else {
			n = nn;
			i = 0;
			nch = nchildren(n);
			rp = noderepr(n);
		}
		w = Fwidth(rp[i]);
		if (w < 0)
			break;
		len += w;
	}
	return len;
}


/*
 * Move the focus to the next line.
 * NB: This is a building block for use in the 'show' module;
 * it cannot set ep->mode or call higher() properly!
 */

Visible bool
nextline(pp)
	register path *pp;
{
	register node n;
	register node nn;
	register int w;
	register int nch;
	register int i = 0;

	for (;;) {
		n = tree(*pp);
		if (width(n) < 0) {
			nch = nchildren(n);
			while (++i <= nch) {
				nn = child(n, i);
				w = width(nn);
				if (w < 0) {
					downi(pp, i) || Abort();
					n = tree(*pp);
					if (fwidth(noderepr(n)[0]) < 0)
						return Yes;
					nch = nchildren(n);
					i = 0;
				}
			}
		}
		/* Must go upward in the tree */
		i = ichild(*pp);
		if (!up(pp))
			return No;
	}
}


/*
 * Compute the current line number.  If the current node begins with
 * a `newline', add one because the first character is actually
 * on the next line.
 */

Visible int
lineno(ep)
	register environ *ep;
{
	register int y;

	y = -focoffset(ep);
	if (y < 0)
		y = 0;
	if (focchar(ep) == '\n')
		++y;
	return y + Ycoord(ep->focus);
}

/*
 * Similarly, compute the current column number.
 * (Hope the abovementioned trick isn't necessary.)
 */

Visible int
colno(ep)
	environ *ep;
{
	int x= focoffset(ep);

	if (x < 0)
		x= 0; /* In fact, give up */
	return x + Xcoord(ep->focus);
}


/*
 * Make the focus exactly one line wide (if at all possible).
 */

Visible Procedure
oneline(ep)
	register environ *ep;
{
	register node n;
	node nn;
	register string *rp;
	register int s1;
	register int s2;
	register int len;
	int ich;
	int nch;

	ich = 1;
	while (width(tree(ep->focus)) >= 0) {
		ich = ichild(ep->focus);
		if (!up(&ep->focus)) {
			ep->mode = WHOLE;
			higher(ep);
			return;
		}
	}
	higher(ep);
	n = tree(ep->focus);
	nch = nchildren(n);
	rp = noderepr(n);
	for (s1 = 2*ich-1; s1 >= 1; --s1) {
		if (s1&1)
			len = fwidth(rp[s1/2]);
		else {
			nn = child(n, s1/2);
			len = width(nn);
		}
		if (len < 0)
			break;
	}
	for (s2 = 2*ich+1; s2 <= 2*nch+1; ++s2) {
		if (s2&1)
			len = fwidth(rp[s2/2]);
		else {
			nn = child(n, s2/2);
			len = width(nn);
		}
		if (len < 0)
			break;
	}
	ep->mode = SUBSET;
	ep->s1 = s1+1;
	ep->s2 = s2-1;
}
