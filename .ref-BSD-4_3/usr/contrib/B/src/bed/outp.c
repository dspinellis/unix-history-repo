/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: outp.c,v 2.4 85/08/22 16:05:48 timo Exp $";

/*
 * B editor -- Screen management package, lower level output part.
 */

#include <ctype.h>

#include "b.h"
#include "bobj.h"
#include "node.h"
#include "supr.h"
#include "gram.h"
#include "cell.h"


#define SOBIT 0200
#define CHAR 0177


/*
 * Variables used for communication with outfocus.
 */

Hidden node thefocus;
Hidden environ wherebuf;
Hidden environ *where = &wherebuf;
Hidden bool realvhole;
Hidden int multiline; /* Height of focus */
Hidden int yfocus;

Visible int focy; /* Where the cursor must go */
Visible int focx;


/*
 * Save position of the focus for use by outnode/outfocus.
 */

Visible Procedure
savefocus(ep)
	register environ *ep;
{
	register int sym;
	register int w;

	realvhole = No;
	thefocus = Nnil;
	multiline = 0;
	yfocus = Ycoord(ep->focus);
	w = focoffset(ep);
	if (w < 0)
		yfocus += -w;
	w = focwidth(ep);
	if (w < 0) {
		multiline = -w;
		if (focchar(ep) == '\n')
			++yfocus;
		else
			++multiline;
		return;
	}
	if (ep->mode == WHOLE) {
		sym = symbol(tree(ep->focus));
		if (sym == Optional)
			ep->mode = ATBEGIN;
	}
	switch(ep->mode) {
	case VHOLE:
		if (ep->s1&1)
			ep->mode = FHOLE;
	case ATBEGIN:
	case ATEND:
	case FHOLE:
		ritevhole(ep);
		switch (ep->mode) {
		case ATBEGIN:
		case FHOLE:
			sym = symbol(tree(ep->focus));
			if (sym == Hole && (ep->mode == ATBEGIN || ep->s2 == 0)) {
				ep->mode = WHOLE;
				break;
			}
			/* Fall through */
		case VHOLE:
		case ATEND:
			leftvhole(ep);
			realvhole = 1 + ep->spflag;
		}
	}
	touchpath(&ep->focus); /* Make sure it is a unique pointer */
	thefocus = tree(ep->focus); /* No copy; used for comparison only! */
	where->mode = ep->mode;
	where->s1 = ep->s1;
	where->s2 = ep->s2;
	where->s3 = ep->s3;
	where->spflag = ep->spflag;
}


/*
 * Incorporate the information saved about the focus.
 */

Visible Procedure
setfocus(tops)
	register cell *tops;
{
	register cell *p;
	register int i;

	for (p = tops, i = 0; i < yfocus; ++i, p = p->c_link) {
		if (!p) {
#ifndef NDEBUG
			debug("[Focus lost (setfocus)]");
#endif NDEBUG
			return;
		}
	}
	p->c_newvhole = realvhole;
	i = multiline;
	do {
		p->c_newfocus = Yes;
		p = p->c_link;
	} while (--i > 0);
}


/*
 * Signal that actual updata is started.
 */

Visible Procedure
startactupdate(nofocus)
	bool nofocus;
{
	if (nofocus) {
		multiline = 0;
		thefocus = Nnil;
	}
}


/*
 * Signal the end of the actual update.
 */

Visible Procedure
endactupdate()
{
}


/*
 * Output a line of text.
 */

Visible Procedure
outline(p, lineno)
	register cell *p;
	register int lineno;
{
	register node n = p->c_data;
	register int w = width(n);
	register string buf =
	    malloc((unsigned) (p->c_newindent + 4 + (w < 0 ? linelen(n) : w)));
			/* some 4 extra for spflag and vhole */
	auto string bp = buf;
	register int i;
	register int endarea = lineno+Space(p)-1;

	if (endarea >= winheight)
		endarea = winheight-1;
	for (i = p->c_newindent; i-- > 0; )
		*bp++ = ' ';
	if (!p->c_newfocus) {
		smash(&bp, n, 0);
		*bp = 0;
	}
	else {
		if (multiline)
			smash(&bp, n, SOBIT);
		else if (n == thefocus)
			focsmash(&bp, n);
		else
			smash(&bp, n, 0);
		*bp = 0;
		for (bp = buf; *bp && !(*bp&SOBIT); ++bp)
			;
		if (*bp&SOBIT) {
			if (focy == Nowhere) {
				focx = indent + bp-buf;
				focy = lineno + focx/llength;
				focx %= llength;
			}
			if (multiline <= 1 && !(bp[1]&SOBIT))
				*bp &= ~SOBIT; /* Clear mask if just one char in focus */
		}
	}
	trmputdata(lineno, endarea, indent, buf);
}


/*
 * Smash -- produce a linear version of a node in a buffer (which had
 * better be long enough!).  The buffer pointer is moved to the end of
 * the resulting string.
 * Care is taken to represent the focus.
 * Characters in the focus have their upper bit set.
 */

#define Outvhole() \
	(where->spflag && strsmash(pbuf, " ", 0), strsmash(pbuf, "?", SOBIT))

Hidden Procedure
focsmash(pbuf, n)
	string *pbuf;
	node n;
{
	value v;
	string str;
	register string *rp;
	register int maxs2;
	register int i;
	register bool ok;
	register int j;
	register int mask;

	switch (where->mode) {

	case WHOLE:
		smash(pbuf, n, SOBIT);
		break;

	case ATBEGIN:
		Outvhole();
		smash(pbuf, n, 0);
		break;

	case ATEND:
		smash(pbuf, n, 0);
		Outvhole();
		break;

	case VHOLE:
		if (!(where->s1&1)) {
			v = (value) child(n, where->s1/2);
			Assert(Type(v) == Tex);
			subsmash(pbuf, Str(v), where->s2, 0);
			Outvhole();
			strsmash(pbuf, Str(v) + where->s2, 0);
			break;
		}
		/* Else, fall through */
	case FHOLE:
		rp = noderepr(n);
		maxs2 = 2*nchildren(n) + 1;
		for (ok = Yes, i = 1; ok && i <= maxs2; ++i) {
			if (i&1) {
				if (i == where->s1) {
					subsmash(pbuf, rp[i/2], where->s2, 0);
					Outvhole();
					if (rp[i/2])
						strsmash(pbuf, rp[i/2] + where->s2, 0);
				}
				else
					strsmash(pbuf, rp[i/2], 0);
			}
			else
				ok = chismash(pbuf, n, i/2, 0);
		}
		break;

	case SUBRANGE:
		rp = noderepr(n);
		maxs2 = 2*nchildren(n) + 1;
		for (ok = Yes, i = 1; ok && i <= maxs2; ++i) {
			if (i&1) {
				if (i == where->s1) {
					subsmash(pbuf, rp[i/2], where->s2,0);
					if (rp[i/2])
						subsmash(pbuf, rp[i/2] + where->s2,
							where->s3 - where->s2 + 1, SOBIT);
					if (rp[i/2])
						strsmash(pbuf, rp[i/2] + where->s3 + 1, 0);
				}
				else
					strsmash(pbuf, rp[i/2], 0);
			}
			else if (i == where->s1) {
				v = (value)child(n, i/2);
				Assert(Type(v) == Tex);
				str = Str(v);
				subsmash(pbuf, str, where->s2, 0);
				subsmash(pbuf, str + where->s2, where->s3 - where->s2 + 1,
					SOBIT);
				strsmash(pbuf, str + where->s3 + 1, 0);
			}
			else
				ok = chismash(pbuf, n, i/2, 0);
		}
		break;

	case SUBLIST:
		for (ok = Yes, j = where->s3; j > 0; --j) {
			rp = noderepr(n);
			maxs2 = 2*nchildren(n) - 1;
			for (i = 1; ok && i <= maxs2; ++i) {
				if (i&1)
					strsmash(pbuf, rp[i/2], SOBIT);
				else
					ok = chismash(pbuf, n, i/2, SOBIT);
			}
			if (ok)
				n = lastchild(n);
		}
		if (ok)
			smash(pbuf, n, 0);
		break;

	case SUBSET:
		rp = noderepr(n);
		maxs2 = 2*nchildren(n) + 1;
		mask = 0;
		for (ok = Yes, i = 1; ok && i <= maxs2; ++i) {
			if (i == where->s1)
				mask = SOBIT;
			if (i&1)
				strsmash(pbuf, rp[i/2], mask);
			else
				ok = chismash(pbuf, n, i/2, mask);
			if (i == where->s2)
				mask = 0;
		}
		break;

	default:
		Abort();
	}
}

Hidden Procedure
smash(pbuf, n, mask)
	register string *pbuf;
	register node n;
	register int mask;
{
	register string *rp;
	register int i;
	register int nch;

	rp = noderepr(n);
	strsmash(pbuf, rp[0], mask);
	nch = nchildren(n);
	for (i = 1; i <= nch; ++i) {
		if (!chismash(pbuf, n, i, mask))
			break;
		strsmash(pbuf, rp[i], mask);
	}
}

Hidden Procedure
strsmash(pbuf, str, mask)
	register string *pbuf;
	register string str;
	register int mask;
{
	if (!str)
		return;
	for (; *str; ++str) {
		if (isprint(*str) || *str == ' ')
			**pbuf = *str|mask, ++*pbuf;
	}
}

Hidden Procedure
subsmash(pbuf, str, len, mask)
	register string *pbuf;
	register string str;
	register int len;
	register int mask;
{
	if (!str)
		return;
	for (; len > 0 && *str; --len, ++str) {
		if (isprint(*str) || *str == ' ')
			**pbuf = *str|mask, ++*pbuf;
	}
}


/*
 * Smash a node's child.
 * Return No if it contained a newline (to stop the parent).
 */

Hidden bool
chismash(pbuf, n, i, mask)
	register string *pbuf;
	register node n;
	register int i;
{
	register node nn = child(n, i);
	register int w;

	if (Type(nn) == Tex) {
		strsmash(pbuf, Str((value)nn), mask);
		return Yes;
	}
	w = width(nn);
	if (w < 0 && Fw_negative(noderepr(nn)[0]))
		return No;
	if (nn == thefocus)
		focsmash(pbuf, nn);
	else
		smash(pbuf, nn, mask);
	return w >= 0;
}
