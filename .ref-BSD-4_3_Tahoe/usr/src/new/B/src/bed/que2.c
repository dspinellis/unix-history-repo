/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: que2.c,v 2.3 84/07/23 13:02:38 guido Exp $";

/*
 * B editor -- Manipulate queues of nodes, higher levels.
 */

#include <ctype.h>

#include "b.h"
#include "feat.h"
#include "bobj.h"
#include "node.h"
#include "supr.h"
#include "queu.h"
#include "gram.h"
#include "tabl.h"


extern bool lefttorite;
	/* Set by edit() to signal we parse purely left-to-right */
extern bool dflag; /* Debug mode even if NDEBUG on */


/*
 * Insert a queue of nodes at the focus
 * (which had better be some kind of a hole).
 * The nodes may also be a text, in which case the individual characters
 * are inserted.
 * Extensive changes to the parse tree may occur, and the node may be
 * broken up in its constituent parts (texts and other nodes) which
 * are then inserted individually.
 */

Visible bool
ins_queue(ep, pq, pq2)
	register environ *ep;
	register queue *pq;
	register queue *pq2;
{
	register bool ok = Yes;
	register node n;
	register queue oldq2;
	environ saveenv;
	int oldindentation = focindent(ep);
	int indentation = oldindentation;

	leftvhole(ep);
	while (ok && !emptyqueue(*pq)) {
		n = queuebehead(pq);
		if (Type(n) == Tex) {
			ok = ins_string(ep, Str((value) n), pq2, 0);
			switch (Str((value) n)[Length((value) n) - 1]) { /* Last char */
			case '\t':
				++indentation;
				break;
			case '\b':
				--indentation;
				break;
			case '\n':
				while (focindent(ep) > indentation) {
					if (!ins_newline(ep))
						break;
				}
				break;
			}
		}
		else {
			Ecopy(*ep, saveenv);
			oldq2 = qcopy(*pq2);
			if (!ins_node(&saveenv, n, pq2)) {
				Erelease(saveenv);
				qrelease(*pq2);
				*pq2 = oldq2;
				if (symbol(n) == Hole)
					ok = ins_string(ep, "?", pq2, 0);
				else
					splitnode(n, pq);
			}
			else {
				Erelease(*ep);
				Emove(saveenv, *ep);
				qrelease(oldq2);
			}
		}
		noderelease(n);
	}
	if (!ok)
		qshow(*pq, "ins_queue");
	qrelease(*pq);
	for (indentation = focindent(ep);
		indentation > oldindentation; --indentation)
		stringtoqueue("\b", pq2); /* Pass on indentation to outer level */
	return ok;
}


/*
 * Subroutine to insert a queue to the right of the focus
 * without affecting the focus position.
 */

Visible bool
app_queue(ep, pq)
	environ *ep;
	queue *pq;
{
	int where;
	static int markbit = 1; /* To properly handle recursive calls */

	if (emptyqueue(*pq))
		return Yes;
	where = focoffset(ep);
	markbit <<= 1;
	markpath(&ep->focus, markbit);
	if (!ins_queue(ep, pq, pq)) {
		markbit >>= 1;
		return No;
	}
	firstmarked(&ep->focus, markbit) || Abort();
	unmkpath(&ep->focus, markbit);
	markbit >>= 1;
	ep->spflag = No;
	fixfocus(ep, where);
	return Yes;
}


/*
 * Advance to next thing after current position.
 */

Visible bool
move_on(ep)
	register environ *ep;
{
	register node n;
	register string *rp;
	register int sym;
	register int ich = ichild(ep->focus);

	if (!up(&ep->focus))
		return No;
	higher(ep);
	n = tree(ep->focus);
	rp = noderepr(n);
	if (Fw_positive(rp[ich])) {
		ep->mode = FHOLE;
		ep->s1 = 2*ich + 1;
		ep->s2 = 0;
		if (ep->spflag) {
			ep->spflag = No;
			if (rp[ich][0] == ' ') {
				++ep->s2;
				if (fwidth(rp[ich]) > 1)
					return Yes;
			}
			else
				return Yes;
		}
		else
			return Yes;
	}
	if (ich < nchildren(n)) {
		s_downi(ep, ich+1);
		sym = symbol(tree(ep->focus));
		if (sym == Hole || sym == Optional)
			ep->mode = WHOLE;
		else
			ep->mode = ATBEGIN;
		return Yes;
	}
	ep->mode = ATEND;
	return Yes;
}


/*
 * Like move_on but moves through fixed texts, skipping only spaces
 * and empty strings.
 * <<<<< This code is a dinosaur and should be revised. >>>>>
 */

Visible bool
fix_move(ep)
	register environ *ep;
{
	register int ich;
	register int i;
	register string *rp;
	register string cp;

	Assert(ep->mode == FHOLE);

	ich = ep->s1/2;
	rp = noderepr(tree(ep->focus));
	cp = rp[ich];
	if (cp) {
		i = ep->s2;
		Assert(i <= Fwidth(cp));
		if (cp[i] == ' ') {
			do {
				++i;
			} while (cp[i] == ' ');
		}
		if (cp[i] == '\b' || cp[i] == '\t') {
			++i;
			Assert(!cp[i]);
		}
		else if (cp[i]) {
			if (i == ep->s2)
				return No;
			ep->s2 = i;
			return Yes;
		}
	}

	if (ich >= nchildren(tree(ep->focus)))
		ep->mode = ATEND;
	else {
		s_downi(ep, ich+1);
		if (symbol(tree(ep->focus)) == Hole
			|| symbol(tree(ep->focus)) == Optional)
			ep->mode = WHOLE;
		else
			ep->mode = ATBEGIN;
	}
	return Yes;
}


/*
 * Insert a node in the parse tree.
 */

Hidden bool
ins_node(ep, n, pq)
	register environ *ep;
	register node n;
	register queue *pq;
{
	register int sym;
	register node nn;
	register markbits x;
	string *rp;

	if (symbol(n) == Optional)
		return Yes;

	for (;;) {
		switch (ep->mode) {

		case FHOLE:
			if (ep->s2 < lenitem(ep) || !fix_move(ep))
				return No;
			continue;

		case VHOLE:
			if (ep->s2 < lenitem(ep) || !move_on(ep))
				return No;
			continue;

		case ATBEGIN:
			sym = symbol(tree(ep->focus));
			if (sym == Optional || sym == Hole) {
				ep->mode = WHOLE;
				continue;
			}
			x = marks(tree(ep->focus));
			if (joinnodes(&ep->focus, n, tree(ep->focus), No)) {
				if (x) {
					s_downi(ep, 2);
					markpath(&ep->focus, x);
					s_up(ep);
				}
				s_down(ep);
				ep->mode = ATEND;
				leftvhole(ep);
				return Yes;
			}
			nn = tree(ep->focus);
			rp = noderepr(nn);
			if (nchildren(nn) >= 1 && Fw_zero(rp[0])) {
				sym = symbol(firstchild(nn));
				if (sym == Hole || sym == Optional) {
					s_down(ep);
					if (fitnode(&ep->focus, n)) {
						ep->mode = ATEND;
						leftvhole(ep);
						return Yes;
					}
					s_up(ep);
				}
			}
			nn = nodecopy(nn);
			if (!fitnode(&ep->focus, n)) {
				addtoqueue(pq, nn);
				noderelease(nn);
				delfocus(&ep->focus);
				ep->mode = WHOLE;
				continue;
			}
			if (downrite(&ep->focus)) {
				if (Type(tree(ep->focus)) != Tex) {
					sym = symbol(tree(ep->focus));
					if (sym == Hole || sym == Optional) {
						if (fitnode(&ep->focus, nn)) {
							noderelease(nn);
							nn = Nnil;
						}
					}
				}
				else
					up(&ep->focus);
			}
			if (nn) {
				addtoqueue(pq, nn);
				noderelease(nn);
			}
			ep->mode = ATEND;
			leftvhole(ep);
			return Yes;

		case WHOLE:
			sym = symbol(tree(ep->focus));
			Assert(sym == Optional || sym == Hole);
			do {
				higher(ep); /* Only for second time around */
				if (fitnode(&ep->focus, n)) {
					ep->mode = ATEND;
					leftvhole(ep);
					return Yes;
				}
			} while (resttoqueue(&ep->focus, pq));
			ep->mode = ATEND;
			/* Fall through */
		case ATEND:
			do {
				higher(ep); /* Only for second time around */
				if (joinnodes(&ep->focus, tree(ep->focus), n, ep->spflag)) {
					ep->spflag = No;
					leftvhole(ep);
					return Yes;
				}
			} while (resttoqueue(&ep->focus, pq)
				|| move_on(ep) && ep->mode == ATEND);
			return No;

		default:
			return No;

		}
	}
}


/*
 * Insert a string in the parse tree.
 */

#define NEXT (++str, alt_c = 0)

Visible bool
ins_string(ep, str, pq, alt_c)
	register environ *ep;
	/*auto*/ string str;
	register queue *pq;
	int alt_c;
{
	register node nn;
	auto value v;
	char buf[1024];
	register string repr;
	string oldstr;
	register int sym;
	register int len;
	bool interactive = alt_c != 0;

	if (alt_c < 0)
		alt_c = 0;
	while (*str) {
		switch (*str) {

		case '\n':
			if (!ins_newline(ep))
				return No;
			/* Fall through */
		case '\t':
		case '\b':
			NEXT;
			continue;

		}
		switch (ep->mode) {

		case ATBEGIN:
			nn = tree(ep->focus);
			if (Type(nn) == Tex) {
				ep->s1 = 2*ichild(ep->focus);
				ep->s2 = 0;
				ep->mode = VHOLE;
				s_up(ep);
				continue;
			}
			sym = symbol(nn);
			if (sym != Optional && sym != Hole) {
				if (fwidth(noderepr(nn)[0]) == 0) {
					if (down(&ep->focus))
						break;
				}
				addtoqueue(pq, nn);
				delfocus(&ep->focus);
			}
			ep->mode = WHOLE;
			/* Fall through */
		case WHOLE:
			nn = tree(ep->focus);
			sym = symbol(nn);
			Assert(sym == Hole || sym == Optional);
			while ((len = fitstring(&ep->focus, str, alt_c)) == 0) {
				if (sym == Optional) {
					if (!move_on(ep)) {
						if (*str == ' ')
							NEXT;
						else
							return No;
					}
					break;
				}
				if (!interactive && *str == '?') {
					NEXT;
					ep->mode = ATEND;
					break;
				}
				if (resttoqueue(&ep->focus, pq))
					higher(ep);
				else if (spacefix(ep))
					break;
				else if (*str == ' ') {
					NEXT;
					break;
				}
				else if (interactive)
					return No;
				else {
					ep->mode = ATEND;
					break;
				}
			}
			if (len > 0) {
				str += len;
				alt_c = 0;
				fixfocus(ep, len);
			}
			break;

		case ATEND:
			if (add_string(ep, &str, alt_c)) {
				alt_c = 0;
				break;
			}
			len = joinstring(&ep->focus, str, ep->spflag,
				alt_c ? alt_c : interactive ? -1 : 0, Yes);
			if (len > 0) {
				s_downi(ep, 2);
				ep->spflag = No;
				fixfocus(ep, len);
			}
			else {
				if (resttoqueue(&ep->focus, pq)) {
					higher(ep);
					break;
				}
				if (move_on(ep))
					break;
				if (*str == ' ') {
					NEXT;
					break;
				}
				return No;
			}
			str += len;
			alt_c = 0;
			break;

		case FHOLE:
			nn = tree(ep->focus);
			repr = noderepr(nn)[ep->s1/2];
			if (ep->s2 >= fwidth(repr)
				&& (ep->s2 <= 0 || ep->spflag || !isalpha(repr[0])
					|| repr[ep->s2-1] == ' ')) { /* At end */
				if (ep->s1/2 < nchildren(nn)) {
					s_downi(ep, ep->s1/2 + 1);
					ep->mode = ATBEGIN; /* Of next child */
				}
				else
					ep->mode = ATEND;
				break;
			}
			if ((*str == ':' || *str == ' ') && *str == repr[ep->s2]) {
				/*****
				 * Quick hack for insertion of test-suites and refinements:
				 *****/
				++ep->s2;
				NEXT;
				continue;
			}
			if (!lefttorite)
				nosuggtoqueue(ep, pq);
			oldstr = str;
			if (resuggest(ep, &str, alt_c) || soften(ep, &str, alt_c)) {
				if (str > oldstr)
					alt_c = 0;
				continue;
			}
			if (fix_move(ep))
				continue;
			return No;

		case VHOLE:
			Assert(!(ep->s1&1));
			nn = tree(ep->focus);
#ifdef USERSUGG
			if (symbol(nn) == Suggestion) {
				if (newsugg(ep, &str, alt_c))
					alt_c = 0;
				else
					killsugg(ep);
				continue;
			}
#endif USERSUGG
			s_downi(ep, ep->s1/2);
			v = copy((value) tree(ep->focus));
			len = 0;
			if (!ep->spflag) {
				for (; len < sizeof buf - 1 && str[len]
						&& mayinsert(nn, ep->s1/2, !!(ep->s2 + len),
							str[len]);
					++len) {
					buf[len] = str[len];
				}
				if (len <= 0 && alt_c
					&& mayinsert(nn, ep->s1/2, !!(ep->s2 + len), alt_c)) {
					buf[0] = alt_c;
					len = 1;
				}
			}
			if (len > 0) { /* Effectuate change */
				str += len;
				alt_c = 0;
				Assert(Type(v) == Tex);
				buf[len] = 0;
				putintrim(&v, ep->s2, Length(v) - ep->s2, buf);
				replace(&ep->focus, (node) v);
				s_up(ep);
				ep->spflag = No;
				ep->s2 += len;
			}
			else { /* Nothing inserted */
				if (ep->s2 == 0) { /* Whole string rejected */
					addtoqueue(pq, (node)v);
					release(v);
					s_up(ep);
					delfocus(&ep->focus);
					ep->mode = WHOLE;
					break;
				}
				if (ep->s2 < Length(v)) {
					addstringtoqueue(pq, Str(v) + ep->s2);
					putintrim(&v, ep->s2, 0, "");
					replace(&ep->focus, (node) v);
				}
				else
					release(v);
				move_on(ep) || Abort(); /* ==> up, cancelling s_downi! */
			}
			break;

		default:
			Abort();

		}
	}

	return Yes;
}


/*
 * See if two nodes can be joined in a hole.
 * 'Spflag' indicates whether a space must be present between the nodes
 * (required or forbidden).
 * Either of n1, n2 may actually be the current contents of the hole.
 */

Hidden bool
joinnodes(pp, n1, n2, spflag)
	path *pp;
	node n1;
	node n2;
	bool spflag;
{
	path pa = parent(*pp);
	int sympa = pa ? symbol(tree(pa)) : Rootsymbol;
	struct table *tp = &table[sympa];
	struct classinfo *ci = tp->r_class[ichild(*pp) - 1];
	classptr cp = ci->c_join;
	int sym1 = symbol(n1);
	int sym2 = symbol(n2);
	int symcp;
	int symfound = -1;

	if (!cp)
		return No;
	for (; *cp; cp += 2) {
		if (cp[0] != spflag + 1)
			continue;
		symcp = cp[1];
		tp = &table[symcp];
		if (isinclass(sym1, tp->r_class[0])
			&& isinclass(sym2, tp->r_class[1])) {
			symfound = symcp;
			break;
		}
	}

	if (symfound < 0)
		return No;
	n1 = nodecopy(n1);
	n2 = nodecopy(n2); /* 'Cause one of them may overlap tree(*pp) */
	replace(pp, table[symfound].r_node);
	down(pp) || Abort();
	replace(pp, n1);
	rite(pp) || Abort();
	replace(pp, n2);
	up(pp) || Abort();
	return Yes;
}


/*
 * Try to join a node (implicit as tree(*pp)) with some text.
 * That is, try to replace the node by one with it as first child,
 * (some of) the text as second child, and nothing or a space in between.
 *
 * 'Spflag' indicates whether a space is desirable between the nodes
 * (but if No it is only used as advice).
 *
 * Returns the number of characters consumed from str.
 */

Visible int
joinstring(pp, str, spflag, alt_c, mayindent)
	path *pp;
	register string str;
	register bool spflag;
	int alt_c;
	bool mayindent;
{
	register struct table *tp;
	path pa = parent(*pp);
	node n1;
	struct classinfo *ci;
	register classptr cp;
	int sympa = pa ? symbol(tree(pa)) : Rootsymbol;
	register int sym1;
	register int symcp;
	int symfound;
	int len;
	char buf[2];
	bool interactive = alt_c != 0;

	if (alt_c < 0)
		alt_c = 0;
	ci = table[sympa].r_class[ichild(*pp) - 1];
	Assert(ci);
	cp = ci->c_join;
	if (!cp)
		return 0;

	n1 = tree(*pp);
	sym1 = symbol(n1);
	symfound = -1;
	for (; *cp; cp += 2) {
		if (cp[0] < spflag + 1)
			continue;
		symcp = cp[1];
		tp = &table[symcp];
		if (!mayindent && tp->r_repr[1] && index(tp->r_repr[1], '\t'))
			continue;
		if (isinclass(sym1, tp->r_class[0])
			&& ((canfitchar(str[0], tp->r_class[1]))
				|| str[0] == '?' && !interactive)) {
			if (cp[0] == spflag + 1) {
				symfound = symcp;
				break;
			}
			if (symfound < 0)
				symfound = symcp;
		}
	}

	if (symfound < 0) { /* 1-level recursion */
		if (!alt_c)
			return 0;
		buf[0] = alt_c;
		buf[1] = 0;
		return joinstring(pp, buf, spflag, 0, mayindent);
	}
	n1 = nodecopy(n1); /* 'Cause it overlaps tree(*pp) */
	replace(pp, table[symfound].r_node);
	down(pp) || Abort();
	replace(pp, n1);
	rite(pp) || Abort();
	len = fitstring(pp, str, 0);
	if (len == 0 && str[0] == '?')
		len = 1;
	Assert(len > 0); /* Disagreement between canfitchar and fitstring */
	up(pp) || Abort();
	return len;
}


/*
 * Similar to joinstring, but now the string must match the delimiter
 * rather than being acceptable as second child.
 * (Interface has changed to resemble resuggest/soften.)
 */

Hidden bool
add_string(ep, pstr, alt_c)
	environ *ep;
	string *pstr;
	int alt_c; /* Yet unused */
{
	register struct table *tp;
	path pa = parent(ep->focus);
	node n1;
	struct classinfo *ci;
	register classptr cp;
	int sympa = pa ? symbol(tree(pa)) : Rootsymbol;
	register int sym1;
	register int symcp;
	register int c;

	ci = table[sympa].r_class[ichild(ep->focus) - 1];
	Assert(ci);
	cp = ci->c_append;
	if (!cp)
		return No;
	n1 = tree(ep->focus);
	sym1 = symbol(n1);
	c = **pstr;
	for (; *cp; cp += 2) {
		if ((*cp&0177) != c)
			continue;
		symcp = cp[1];
		tp = &table[symcp];
		if (isinclass(sym1, tp->r_class[0]))
			break;
	}
	if (!*cp)
		return No;
	++*pstr;
	if (c == ' ') {
		ep->spflag = Yes;
		return Yes;
	}
	n1 = nodecopy(n1); /* 'Cause it overlaps tree(ep->focus) */
	replace(&ep->focus, table[symcp].r_node);
	s_down(ep);
	replace(&ep->focus, n1);
	s_up(ep);
	ep->mode = FHOLE;
	ep->s1 = 3;
	ep->s2 = (*cp&0200) ? 2 : 1;
	ep->spflag = No;
	return Yes;
}


/*
 * See whether a character may start a new node in a hole with given class.
 */

Visible bool
canfitchar(c, ci)
	int c;
	struct classinfo *ci;
{
	register classptr cp;
	register int code = Code(c);

	Assert(ci);
	cp = ci->c_insert;
	Assert(cp);
	for (; *cp; cp += 2) {
		if (cp[0] == code)
			return Yes;
	}
	return No;
}


/*
 * Debug routine to print a queue.
 */

Visible Procedure
qshow(q, where)
	queue q;
	string where;
{
#ifndef NDEBUG
	node n;
	char buf[256];
	string cp;
	string sp;

	sprintf(buf, "%s:", where);
	cp = buf + strlen(buf);
	for (;q; q = q->q_link) {
		n = q->q_data;
		*cp++ = ' ';
		if (Type(n) == Tex) {
			*cp++ = '"';
			for (sp = Str((value) n); *sp; ++sp) {
				if (isprint(*sp) || *sp == ' ') {
					*cp++ = *sp;
					if (*sp == '"')
						*cp++ = *sp;
				}
				else {
					sprintf(cp, "\\%03o", *sp&0377);
					cp += 4;
				}
			}
			*cp++ = '"';
		}
		else {
			strncpy(cp, table[symbol(n)].r_name, 80);
			cp += strlen(cp);
		}
		if (cp >= buf+80) {
			strcpy(buf+76, "...");
			break;
		}
	}
	*cp = 0;
	debug(buf);
#endif NDEBUG
}
