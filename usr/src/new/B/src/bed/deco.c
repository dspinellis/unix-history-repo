/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: deco.c,v 2.3 84/07/19 11:45:12 guido Exp $";

/*
 * B editor -- Delete and copy commands.
 */

#include <ctype.h>

#include "b.h"
#include "erro.h"
#include "bobj.h"
#include "node.h"
#include "gram.h"
#include "supr.h"
#include "queu.h"


value copyout(); /* Forward */

/*
 * DELETE and COPY currently share a buffer, called the copy buffer.
 * (Physically, there is one such a buffer in each environment.)
 * In ordinary use, the copy buffer receives the text deleted by the
 * last DELETE command (unless it just removed a hole); the COPY command
 * can then be used (with the focus on a hole) to copy it back.
 * When some portion of text must be held while other text is deleted,
 * the COPY command again, but now with the focus on the text to be held,
 * copies it to the buffer and deleted text won't overwrite the buffer
 * until it is copied back at least once.
 * If the buffer holds text that was explicitly copied out but not yet
 * copied back in, it is saved on a file when the editor exits, so it can
 * be used in the next session; but this is not true for text implicitly
 * placed in the buffer through DELETE.
 */

/*
 * Delete command -- delete the text in the focus, or delete the hole
 * if it is only a hole.
 */

Visible bool
delete(ep)
	register environ *ep;
{
	higher(ep);
	shrink(ep);
	if (ishole(ep))
		return delhole(ep);
	if (!ep->copyflag) {
		release(ep->copybuffer);
		ep->copybuffer = copyout(ep);
	}
	return delbody(ep);
}


/*
 * Delete the focus under the assumption that it contains some text.
 */

Visible bool
delbody(ep)
	register environ *ep;
{
	ep->changed = Yes;

	subgrow(ep, No); /* Don't ignore spaces */
	switch (ep->mode) {

	case SUBRANGE:
		if (ep->s1&1)
			return delfixed(ep);
		return delvarying(ep);

	case SUBSET:
		return delsubset(ep, Yes);

	case SUBLIST:
		return delsublist(ep);

	case WHOLE:
		return delwhole(ep);

	default:
		Abort();
		/* NOTREACHED */
	}
}


/*
 * Delete portion (ep->mode == SUBRANGE) of varying text ((ep->s1&1) == 0).
 */

Hidden bool
delvarying(ep)
	register environ *ep;
{
	auto queue q = Qnil;
	register node n = tree(ep->focus);
	auto value v = (value) child(n, ep->s1/2);
	register len = Length(v);

	Assert(ep->mode == SUBRANGE && !(ep->s1&1)); /* Wrong call */
	Assert(Type(v) == Tex); /* Inconsistent parse tree */
	if (ep->s2 == 0
		&& !mayinsert(tree(ep->focus), ep->s1/2, 0, Str(v)[ep->s3 + 1])) {
		/* Cannot do simple substring deletion. */
		stringtoqueue(Str(v) + ep->s3 + 1, &q);
		delfocus(&ep->focus);
		ep->mode = WHOLE;
		return app_queue(ep, &q);
	}
	v = copy(v);
	putintrim(&v, ep->s2, len - ep->s3 - 1, "");
	s_downi(ep, ep->s1/2);
	replace(&ep->focus, (node) v);
	s_up(ep);
	ep->mode = VHOLE;
	return Yes;
}


/*
 * Delete portion (ep->mode == SUBRANGE) of fixed text ((ep->s1&1) == 1).
 */

Hidden bool
delfixed(ep)
	register environ *ep;
{
	register node n = tree(ep->focus);
	char buf[15]; /* Long enough for all fixed texts */
	register string repr = noderepr(n)[ep->s1/2];
	register int len;
	queue q = Qnil;
	bool ok;

	Assert(ep->mode == SUBRANGE && (ep->s1&1));
	if (ep->s1 > 1) {
		ep->mode = FHOLE;
		return Yes;
	}
	Assert(fwidth(repr) < sizeof buf - 1);
	len = ep->s2;
	ep->s2 = ep->s3 + 1;
	ep->mode = FHOLE;
	nosuggtoqueue(ep, &q);
	strcpy(buf, repr);
	if (nchildren(tree(ep->focus)) > 0)
		buf[len] = 0;
	else
		strcpy(buf+len, buf+ep->s2);
	delfocus(&ep->focus);
	ep->mode = WHOLE;
	markpath(&ep->focus, 1);
	ok = ins_string(ep, buf, &q, 0);
	if (!ok) {
		qrelease(q);
		return No;
	}
	firstmarked(&ep->focus, 1) || Abort();
	unmkpath(&ep->focus, 1);
	fixfocus(ep, len);
	return app_queue(ep, &q);
}


/*
 * Delete focus if ep->mode == SUBSET.
 */

Hidden bool
delsubset(ep, hack)
	register environ *ep;
	bool hack;
{
	auto queue q = Qnil;
	auto queue q2 = Qnil;
	register node n = tree(ep->focus);
	register node nn;
	register string *rp = noderepr(n);
	register int nch = nchildren(n);
	register int i;

	if (hack) {
		shrsubset(ep);
		if (ep->s1 == ep->s2 && !(ep->s1&1)) {
			nn = child(tree(ep->focus), ep->s1/2);
			if (fwidth(noderepr(nn)[0]) < 0) {
				/* It starts with a newline, leave the newline */
				s_downi(ep, ep->s1/2);
				ep->mode = SUBSET;
				ep->s1 = 2;
				ep->s2 = 2*nchildren(nn) + 1;
				return delsubset(ep, hack);
			}
		}
		subgrsubset(ep, No); /* Undo shrsubset */
		if (ep->s2 == 3 && rp[1] && Strequ(rp[1], "\t"))
			--ep->s2; /* Hack for deletion of unit-head or if/for/wh. head */
	}
	if (ep->s1 == 1 && Fw_negative(rp[0]))
		++ep->s1; /* Hack for deletion of test-suite or refinement head */

	if (Fw_zero(rp[0]) ? (ep->s2 < 3 || ep->s1 > 3) : ep->s1 > 1) {
		/* No deep structural change */
		for (i = (ep->s1+1)/2; i <= ep->s2/2; ++i) {
			s_downi(ep, i);
			delfocus(&ep->focus);
			s_up(ep);
		}
		if (ep->s1&1) {
			ep->mode = FHOLE;
			ep->s2 = 0;
		}
		else if (Type(child(tree(ep->focus), ep->s1/2)) == Tex) {
			ep->mode = VHOLE;
			ep->s2 = 0;
		}
		else {
			s_downi(ep, ep->s1/2);
			ep->mode = ATBEGIN;
		}
		return Yes;
	}

	balance(ep); /* Make balanced \t - \b pairs */
	subsettoqueue(n, 1, ep->s1-1, &q);
	subsettoqueue(n, ep->s2+1, 2*nch+1, &q2);
	nonewline(&q2); /* Wonder what will happen...? */
	delfocus(&ep->focus);
	ep->mode = ATBEGIN;
	leftvhole(ep);
	if (!ins_queue(ep, &q, &q2)) {
		qrelease(q2);
		return No;
	}
	return app_queue(ep, &q2);
}


/*
 * Delete the focus if ep->mode == SUBLIST.
 */

delsublist(ep)
	register environ *ep;
{
	register node n;
	register int i;
	register int sym;
	queue q = Qnil;
	bool flag;

	Assert(ep->mode == SUBLIST);
	n = tree(ep->focus);
	flag = fwidth(noderepr(n)[0]) < 0;
	for (i = ep->s3; i > 0; --i) {
		n = lastchild(n);
		Assert(n);
	}
	if (flag) {
		n = nodecopy(n);
		s_down(ep);
		do {
			delfocus(&ep->focus);
		} while (rite(&ep->focus));
		if (!allowed(ep->focus, symbol(n))) {
			error(DEL_REM); /* The remains wouldn't fit */
			noderelease(n);
			return No;
		}
		replace(&ep->focus, n);
		s_up(ep);
		s_down(ep); /* I.e., to leftmost sibling */
		ep->mode = WHOLE;
		return Yes;
	}
	sym = symbol(n);
	if (sym == Optional || sym == Hole) {
		delfocus(&ep->focus);
		ep->mode = WHOLE;
	}
	else if (!allowed(ep->focus, sym)) {
		preptoqueue(n, &q);
		delfocus(&ep->focus);
		ep->mode = WHOLE;
		return app_queue(ep, &q);
	}
	else {
		replace(&ep->focus, nodecopy(n));
		ep->mode = ATBEGIN;
	}
	return Yes;
}


/*
 * Delete the focus if ep->mode == WHOLE.
 */

Hidden bool
delwhole(ep)
	register environ *ep;
{
	register int sym = symbol(tree(ep->focus));

	Assert(ep->mode == WHOLE);
	if (sym == Optional || sym == Hole)
		return No;
	delfocus(&ep->focus);
	return Yes;
}


/*
 * Delete the focus if it is only a hole.
 * Assume shrink() has been called before!
 */

Hidden bool
delhole(ep)
	register environ *ep;
{
	node n;
	int sym;
	bool flag = No;

	switch (ep->mode) {
	
	case ATBEGIN:
	case VHOLE:
	case FHOLE:
	case ATEND:
		return widen(ep);

	case WHOLE:
		Assert((sym = symbol(tree(ep->focus))) == Optional || sym == Hole);
		if (ichild(ep->focus) != 1)
			break;
		if (!up(&ep->focus))
			return No;
		higher(ep);
		ep->mode = SUBSET;
		ep->s1 = 2;
		ep->s2 = 2;
		if (fwidth(noderepr(tree(ep->focus))[0]) < 0) {
			flag = Yes;
			ep->s2 = 3; /* Extend to rest of line */
		}
	}

	ep->changed = Yes;
	grow(ep);

	switch (ep->mode) {

	case SUBSET:
		if (!delsubset(ep, No))
			return No;
		if (!flag)
			return widen(ep);
		leftvhole(ep);
		oneline(ep);
		return Yes;

	case SUBLIST:
		n = tree(ep->focus);
		n = lastchild(n);
		sym = symbol(n);
		if (!allowed(ep->focus, sym)) {
			error(DEL_REM); /* The remains wouldn't fit */
			return No;
		}
		flag = samelevel(sym, symbol(tree(ep->focus)));
		replace(&ep->focus, nodecopy(n));
		if (flag) {
			ep->mode = SUBLIST;
			ep->s3 = 1;
		}
		else
			ep->mode = WHOLE;
		return Yes;

	case WHOLE:
		Assert(!parent(ep->focus)); /* Must be at root! */
		return No;

	default:
		Abort();
		/* NOTREACHED */

	}
}


/*
 * Subroutine to delete the focus.
 */

Visible Procedure
delfocus(pp)
	register path *pp;
{
	register path pa = parent(*pp);
	register int sympa = pa ? symbol(tree(pa)) : Rootsymbol;

	replace(pp, child(gram(sympa), ichild(*pp)));
}


/*
 * Copy command -- copy the focus to the copy buffer if it contains
 * some text, copy the copy buffer into the focus if the focus is
 * empty (just a hole).
 */

Visible bool
copyinout(ep)
	register environ *ep;
{
	shrink(ep);
	if (!ishole(ep)) {
		release(ep->copybuffer);
		ep->copybuffer = copyout(ep);
		ep->copyflag = !!ep->copybuffer;
		return ep->copyflag;
	}
	else {
		fixit(ep); /* Make sure it looks like a hole now */
		if (!copyin(ep, (queue) ep->copybuffer))
			return No;
		ep->copyflag = No;
		return Yes;
	}
}


/*
 * Copy the focus to the copy buffer.
 */

Visible value
copyout(ep)
	register environ *ep;
{
	auto queue q = Qnil;
	auto path p;
	register node n;
	register value v;
	char buf[15];
	register string *rp;
	register int i;

	switch (ep->mode) {
	case WHOLE:
		preptoqueue(tree(ep->focus), &q);
		break;
	case SUBLIST:
		p = pathcopy(ep->focus);
		for (i = ep->s3; i > 0; --i)
			downrite(&p) || Abort();
		for (i = ep->s3; i > 0; --i) {
			up(&p) || Abort();
			n = tree(p);
			subsettoqueue(n, 1, 2*nchildren(n) - 1, &q);
		}
		pathrelease(p);
		break;
	case SUBSET:
		balance(ep);
		subsettoqueue(tree(ep->focus), ep->s1, ep->s2, &q);
		break;
	case SUBRANGE:
		Assert(ep->s3 >= ep->s2);
		if (ep->s1&1) { /* Fixed text */
			Assert(ep->s3 - ep->s2 + 1 < sizeof buf);
			rp = noderepr(tree(ep->focus));
			Assert(ep->s2 < Fwidth(rp[ep->s1/2]));
			strncpy(buf, rp[ep->s1/2] + ep->s2, ep->s3 - ep->s2 + 1);
			buf[ep->s3 - ep->s2 + 1] = 0;
			stringtoqueue(buf, &q);
		}
		else { /* Varying text */
			v = (value) child(tree(ep->focus), ep->s1/2);
			Assert(Type(v) == Tex);
			v = trim(v, ep->s2, Length(v) - ep->s3 - 1);
			preptoqueue((node)v, &q);
			release(v);
		}
		break;
	default:
		Abort();
	}
	nonewline(&q);
	return (value)q;
}


/*
 * Subroutine to ensure the copy buffer doesn't start with a newline.
 */

Hidden Procedure
nonewline(pq)
	register queue *pq;
{
	register node n;
	register int c;

	if (!emptyqueue(*pq)) {
		for (;;) {
			n = queuebehead(pq);
			if (Type(n) == Tex) {
				if (Str((value) n)[0] != '\n')
					preptoqueue(n, pq);
				noderelease(n);
				break;
			}
			else {
				c = nodechar(n);
				if (c != '\n')
					preptoqueue(n, pq);
				else
					splitnode(n, pq);
				noderelease(n);
				if (c != '\n')
					break;
			}
		}
	}
}


/*
 * Refinement for copyout, case SUBSET: make sure that \t is balanced with \b.
 * Actually it can only handle the case where a \t is in the subset and the
 * matching \b is immediately following.
 */

Hidden Procedure
balance(ep)
	environ *ep;
{
	string *rp = noderepr(tree(ep->focus));
	int i;
	int level = 0;

	Assert(ep->mode == SUBSET);
	for (i = ep->s1/2; i*2 < ep->s2; ++i) {
		if (rp[i]) {
			if (index(rp[i], '\t'))
				++level;
			else if (index(rp[i], '\b'))
				--level;
		}
	}
	if (level > 0 && i*2 == ep->s2 && rp[i] && index(rp[i], '\b'))
		ep->s2 = 2*i + 1;
}


/*
 * Copy the copy buffer to the focus.
 */

Hidden bool
copyin(ep, q)
	register environ *ep;
	/*auto*/ queue q;
{
	auto queue q2 = Qnil;

	if (!q) {
		error(COPY_EMPTY); /* Empty copy buffer */
		return No;
	}
	ep->changed = Yes;
	q = qcopy(q);
	if (!ins_queue(ep, &q, &q2)) {
		qrelease(q2);
		return No;
	}
	return app_queue(ep, &q2);
}


/*
 * Find out whether the focus looks like a hole or if it has some real
 * text in it.
 * Assumes shrink(ep) has already been performed.
 */

Visible bool
ishole(ep)
	register environ *ep;
{
	register int sym;

	switch (ep->mode) {
	
	case ATBEGIN:
	case ATEND:
	case VHOLE:
	case FHOLE:
		return Yes;

	case SUBLIST:
	case SUBRANGE:
		return No;

	case SUBSET:
		return colonhack(ep); /* (Side-effect!) */

	case WHOLE:
		sym = symbol(tree(ep->focus));
		return sym == Optional || sym == Hole;

	default:
		Abort();
		/* NOTREACHED */
	}
}


/*
 * Amendment to ishole so that it categorizes '?: ?' as a hole.
 * This makes deletion of empty refinements / alternative-suites
 * easier (Steven).
 */

Hidden bool
colonhack(ep)
	environ *ep;
{
	node n = tree(ep->focus);
	node n1;
	string *rp = noderepr(n);
	int i;
	int sym;

	for (i = ep->s1; i <= ep->s2; ++i) {
		if (i&1) {
			if (!allright(rp[i/2]))
				return No;
		}
		else {
			n1 = child(n, i/2);
			if (Type(n1) == Tex)
				return No;
			sym = symbol(n1);
			if (sym != Hole && sym != Optional)
				return No;
		}
	}
	return Yes;
}


/*
 * Refinement for colonhack.  Recognize strings that are almost blank
 * (i.e. containing only spaces, colons and the allowed control characters).
 */

Hidden bool
allright(repr)
	string repr;
{
	if (repr) {
		for (; *repr; ++repr) {
			if (!index(": \t\b\n\r", *repr))
				return No;
		}
	}
	return Yes;
}
