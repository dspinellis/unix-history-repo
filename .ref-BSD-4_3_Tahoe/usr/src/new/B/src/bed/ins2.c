/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: ins2.c,v 2.4 84/10/26 12:08:24 guido Exp $";

/*
 * B editor -- Insert characters from keyboard.
 */

#include "b.h"
#include "bobj.h"
#include "node.h"
#include "supr.h"
#include "queu.h"
#include "gram.h"
#include "tabl.h"


/*
 * Insert a character.
 */

Visible bool
ins_char(ep, c, alt_c)
	register environ *ep;
	int c;
	int alt_c;
{
	auto queue q = Qnil;
	auto queue qf = Qnil;
	auto value copyout();
	auto string str;
	char buf[2];
	int where;
	bool spwhere;

	higher(ep);
	shrink(ep);
	if (index("({[`'\"", c) && !ishole(ep)) {
		/* Surround something.  Wonder what will happen! */
		qf = (queue) copyout(ep);
		if (!delbody(ep)) {
			qrelease(qf);
			return No;
		}
	}
	fixit(ep);
	ep->changed = Yes;
	buf[0] = c;
	buf[1] = 0;
	if (!ins_string(ep, buf, &q, alt_c))
		return No;
	if (!emptyqueue(q) || !emptyqueue(qf)) {
		/* Slight variation on app_queue */
		if (!emptyqueue(qf) && emptyqueue(q))
			ritevhole(ep); /* Wizardry.  Why does this work? */
		spwhere = ep->spflag;
		ep->spflag = No;
		where = focoffset(ep);
		markpath(&ep->focus, 1);
		ep->spflag = spwhere;
		if (ep->mode == FHOLE && ep->s2 > 0) {
			/* If we just caused a suggestion, insert the remains
			   after the suggested text, not after its first character. */
			str = "";
			if (!soften(ep, &str, 0)) {
				ep->mode = ATEND;
				leftvhole(ep);
				if (symbol(tree(ep->focus)) == Hole) {
					ep->mode = ATBEGIN;
					leftvhole(ep);
				}
			}
		}
		if (!emptyqueue(q)) { /* Re-insert stuff queued by ins_string */
			if (!ins_queue(ep, &q, &q))
				return No;
			where += spwhere;
			spwhere = No;
		}
		if (!emptyqueue(qf)) { /* Re-insert deleted old focus */
			firstmarked(&ep->focus, 1) || Abort();
			fixfocus(ep, where);
			if (!ins_queue(ep, &qf, &qf))
				return No;
		}
		firstmarked(&ep->focus, 1) || Abort();
		unmkpath(&ep->focus, 1);
		ep->spflag = No;
		fixfocus(ep, where + spwhere);
	}
	return Yes;
}


/*
 * Insert a newline.
 */

Visible bool
ins_newline(ep)
	register environ *ep;
{
	register node n;
	register int sym;
	auto bool mayindent;

	ep->changed = Yes;
	if (!fiddle(ep, &mayindent))
		return No;
	for (;;) {
		switch (ep->mode) {

		case VHOLE:
			ep->mode = ATEND;
			continue;

		case FHOLE:
			ep->s2 = lenitem(ep);
			if (!fix_move(ep))
				return No;
			continue;

		case ATEND:
			if (!joinstring(&ep->focus, "\n", No, 0, mayindent)) {
				if (!move_on(ep))
					return No;
				continue;
			}
			s_downi(ep, 2);
			s_downi(ep, 1);
			ep->mode = WHOLE;
			Assert((sym = symbol(tree(ep->focus))) == Hole || sym == Optional);
			return Yes;

		case ATBEGIN:
			n = tree(ep->focus);
			if (Type(n) == Tex) {
				ep->mode = ATEND;
				continue;
			}
			sym = symbol(n);
			if (sym == Hole || sym == Optional) {
				ep->mode = WHOLE;
				continue;
			}
			n = nodecopy(n);
			if (!fitstring(&ep->focus, "\n", 0)) {
				if (!down(&ep->focus))
					ep->mode = ATEND;
				noderelease(n);
				continue;
			}
			s_downrite(ep);
			if (fitnode(&ep->focus, n)) {
				noderelease(n);
				s_up(ep);
				s_down(ep);
				ep->mode = WHOLE;
				return Yes;
			}
			s_up(ep);
			s_down(ep);
			if (!fitnode(&ep->focus, n)) {
				noderelease(n);
#ifndef NDEBUG
				debug("[Sorry, I don't see how to insert a newline here]");
#endif NDEBUG
				return No;
			}
			noderelease(n);
			ep->mode = ATBEGIN;
			return Yes;

		case WHOLE:
			Assert((sym = symbol(tree(ep->focus))) == Hole || sym == Optional);
			if (!fitstring(&ep->focus, "\n", 0)) {
				ep->mode = ATEND;
				continue;
			}
			s_downi(ep, 1);
			Assert((sym = symbol(tree(ep->focus))) == Hole || sym == Optional);
			ep->mode = WHOLE;
			return Yes;

		default:
			Abort();

		}
	}
}


/*
 * Refinement for ins_newline() to do the initial processing.
 */

Hidden bool
fiddle(ep, pmayindent)
	register environ *ep;
	bool *pmayindent;
{
	register int level;
	auto string str = "";

	higher(ep);
	while (rnarrow(ep))
		;
	fixit(ep);
	soften(ep, &str, 0);
	higher(ep);
	*pmayindent = Yes;
	if (atdedent(ep)) {
		*pmayindent = No;
		s_up(ep);
		level = Level(ep->focus);
		delfocus(&ep->focus);
		if (symbol(tree(ep->focus)) == Hole) {
			if (hackhack(ep))
				return Yes;
		}
		while (Level(ep->focus) >= level) {
			if (!nexthole(ep)) {
				ep->mode = ATEND;
				break;
			}
		}
		if (ep->mode == ATEND) {
			leftvhole(ep);
			ep->mode = ATEND;
			while (Level(ep->focus) >= level) {
				if (!up(&ep->focus))
					return No;
			}
		}
		return Yes;
	}
	return Yes;
}


/*
 * "Hier komen de houthakkers."
 *
 * Incredibly ugly hack to delete a join whose second child begins with \n,
 * such as a suite after an IF, FOR or WHILE or  unit heading.
 * Inspects the parent node.
 * If this has rp[0] ands rp[1] both empty, replace it by its first child.
 * (caller assures this makes sense).
 * Return Yes if this happened AND rp[1] contained a \t.
 */

Hidden Procedure
hackhack(ep)
	environ *ep;
{
	node n;
	int ich = ichild(ep->focus);
	string *rp;

	if (!up(&ep->focus))
		return No;
	higher(ep);
	rp = noderepr(tree(ep->focus));
	if (!Fw_zero(rp[0]) || !Fw_zero(rp[1])) {
		s_downi(ep, ich);
		return No;
	}
	n = nodecopy(firstchild(tree(ep->focus)));
	delfocus(&ep->focus);
	replace(&ep->focus, n);
	ep->mode = ATEND;
	return rp[1] && rp[1][0] == '\t';
}
	

/*
 * Refinement for fiddle() to find out whether we are at a possible
 * decrease-indentation position.
 */

Hidden bool
atdedent(ep)
	register environ *ep;
{
	register path pa;
	register node npa;
	register int i;
	register int sym = symbol(tree(ep->focus));

	if (sym != Hole && sym != Optional)
		return No;
	if (ichild(ep->focus) != 1)
		return No;
	switch (ep->mode) {
	case FHOLE:
		if (ep->s1 != 1 || ep->s2 != 0)
			return No;
		break;
	case ATBEGIN:
	case WHOLE:
	case SUBSET:
		break;
	default:
		return No;
	}
	pa = parent(ep->focus);
	if (!pa)
		return No;
	npa = tree(pa);
	if (fwidth(noderepr(npa)[0]) >= 0)
		return No;
	for (i = nchildren(npa); i > 1; --i) {
		sym = symbol(child(npa, i));
		if (sym != Hole && sym != Optional)
			return No;
	}
	return Yes; /* Sigh! */
}

/*
 * Refinement for ins_node() and fiddle() to find the next hole,
 * skipping blank space only.
 */

Hidden bool
nexthole(ep)
	register environ *ep;
{
	register node n;
	register int ich;
	register string repr;

	do {
		ich = ichild(ep->focus);
		if (!up(&ep->focus))
			return No;
		higher(ep);
		n = tree(ep->focus);
		repr = noderepr(n)[ich];
		if (!Fw_zero(repr) && !allspaces(repr))
			return No;
	} while (ich >= nchildren(n));
	s_downi(ep, ich+1);
	return Yes;
}
