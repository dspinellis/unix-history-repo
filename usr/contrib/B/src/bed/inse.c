/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: inse.c,v 2.4 85/02/14 13:27:09 timo Exp $";

/*
 * Subroutines (refinements) for ins_string() (see que2.c).
 */

#include "b.h"
#include "feat.h"
#include "bobj.h"
#include "node.h"
#include "gram.h"
#include "supr.h"
#include "tabl.h"

#include <ctype.h>


/*
 * Try to insert the character c in the focus *pp.
 */

Visible bool
insguess(pp, c, ep)
	path *pp;
	char c;
	environ *ep;
{
	path pa = parent(*pp);
	node n;
	int sympa = pa ? symbol(tree(pa)) : Rootsymbol;
	int ich = ichild(*pp);
	struct classinfo *ci = table[sympa].r_class[ich-1];
	classptr cp;
	string *rp;
	int code = Code(c);
	int sym;
	char buf[2];

#ifdef USERSUGG
	if (isinclass(Suggestion, ci)) {
		if (setsugg(pp, c, ep))
			return Yes;
	}
#endif USERSUGG
	for (cp = ci->c_insert; *cp; cp += 2) {
		if (cp[0] == code)
			break;
	}
	if (!*cp)
		return No;
	sym = cp[1];
	if (sym >= LEXICAL) {
		buf[0] = c;
		buf[1] = 0;
		replace(pp, (node) mk_text(buf));
		ep->mode = VHOLE;
		ep->s1 = 2*ich;
		ep->s2 = 1;
		return Yes;
	}
	Assert(sym < TABLEN);
	rp = table[sym].r_repr;
	n = table[sym].r_node;
	if (Fw_zero(rp[0])) {
		buf[0] = c;
		buf[1] = 0;
		setchild(&n, 1, (node) mk_text(buf));
		replace(pp, n);
		ep->mode = VHOLE;
		ep->s1 = 2;
		ep->s2 = 1;
		return Yes;
	}
	replace(pp, n);
	if (c == '\n' || c == '\r') {
		ep->mode = SUBSET;
		ep->s1 = ep->s2 = 2;
	}
	else {
		ep->mode = FHOLE;
		ep->s1 = 1;
		ep->s2 = 1;
	}
	return Yes;
}


/*
 * Test whether character `c' may be inserted in position `s2' in
 * child `ich' of node `n'; that child must be a Text.
 */

Visible bool
mayinsert(n, ich, s2, c)
	node n;
	int ich;
	int s2;
	register char c;
{
	int sympa = symbol(n);
	struct classinfo *ci;
	register classptr cp;
	register value v = (value) child(n, ich);
	register char c1;
	bool maycontinue();
	bool maystart();
	register bool (*fun1)() = s2 > 0 ? maystart : maycontinue;
	register bool (*fun)() = s2 > 0 ? maycontinue : maystart;

	Assert(v && v->type == Tex);
	Assert(sympa > 0 && sympa < TABLEN);
	ci = table[sympa].r_class[ich-1];
	Assert(ci && ci->c_class);
	c1 = Str(v)[0];
	for (cp = ci->c_class; *cp; ++cp) {
		if (*cp >= LEXICAL && (*fun1)(c1, *cp)) {
			if ((*fun)(c, *cp))
				return Yes;
		}
	}
	return No;
}


/*
 * Change a Fixed into a Variable node, given a string pointer variable
 * which contains the next characters to be inserted.
 * If the change is not appropriate, No is returned.
 * Otherwise, as many (though maybe zero) characters from the string
 * as possible will have been incorporated in the string node.
 */

Visible bool
soften(ep, pstr, alt_c)
	environ *ep;
	string *pstr;
	int alt_c;
{
	path pa = parent(ep->focus);
	node n;
	int sympa = pa ? symbol(tree(pa)) : Rootsymbol;
	struct classinfo *ci;
	register classptr cp;
	register int code;
	string repr;
	register struct table *tp;
	char buf[1024];

	if (ep->mode == VHOLE && (ep->s1&1))
		ep->mode = FHOLE;
	if (ep->mode != FHOLE || ep->s1 != 1 || ep->s2 <= 0 || !issuggestion(ep))
		return No;
	n = tree(ep->focus);
	repr = noderepr(n)[0];
	if (!repr || !isupper(repr[0]))
		return No;
	code = Code(repr[0]);
	ci = table[sympa].r_class[ichild(ep->focus) - 1];
	n = Nnil;
	for (cp = ci->c_insert; *cp; cp += 2) {
		if (cp[0] != code)
			continue;
		if (cp[1] >= TABLEN)
			continue;
		tp = &table[cp[1]];
		if (Fw_zero(tp->r_repr[0])) {
			Assert(tp->r_class[0]->c_class[0] >= LEXICAL);
			n = tp->r_node;
			break;
		}
	}
	if (!n)
		return No;
	strncpy(buf, repr, ep->s2);
	buf[ep->s2] = 0;
	setchild(&n, 1, (node) mk_text(buf));
	if (!mayinsert(n, 1, ep->s2, repr[ep->s2])) {
		if (!**pstr || !mayinsert(n, 1, ep->s2, **pstr)
			&& (!alt_c || !mayinsert(n, 1, ep->s2, alt_c))) {
			noderelease(n); /* Don't forget! */
			return No;
		}
	}
	if (**pstr && mayinsert(n, 1, ep->s2, **pstr)) {
		do {
			buf[ep->s2] = **pstr;
			++*pstr;
			++ep->s2;
		} while (ep->s2 < sizeof buf - 1 && **pstr
				&& mayinsert(n, 1, ep->s2, **pstr));
		buf[ep->s2] = 0;
		setchild(&n, 1, (node) mk_text(buf));
	}
	replace(&ep->focus, n);
	ep->mode = VHOLE;
	ep->s1 = 2;
	return Yes;
}


/*
 * Renew suggestion, or advance in old suggestion.
 * Return Yes if *pstr has been advanced.
 */

Visible bool
resuggest(ep, pstr, alt_c)
	environ *ep;
	string *pstr;
	int alt_c;
{
	struct table *tp;
	struct classinfo *ci;
	classptr cp;
	path pa;
	node nn;
	node n = tree(ep->focus);
	register string *oldrp = noderepr(n);
	register int ich = ep->s1/2;
	register string str = oldrp[ich];
	int oldsym = symbol(n);
	int childsym[MAXCHILD];
	string *newrp;
	int sympa;
	register int sym;
	int symfound = -1;
	register int i;
	int code;
	char buf[15]; /* Should be sufficient for all fixed texts */
	bool ok;
	bool anyok = No;

	if (!str || !**pstr || !issuggestion(ep))
		return No;
	/***** Change this if commands can be prefixes of others! *****/
	/***** Well, they can!
	if (!c)
		return No;
		*****/
	if (ich > 0 && ifmatch(ep, pstr, str, alt_c))
		/* Shortcut: sec. keyword, exact match will do just fine */
		return Yes;
	if (ep->s2 <= 0 || Fw_zero(oldrp[0]))
		return No;
	if (**pstr != ' ' && !isupper(**pstr)
		&& !alt_c && **pstr != '"' && **pstr != '\'')
		/* Shortcut: not a keyword, must match exactly */
		return ifmatch(ep, pstr, str, alt_c);
	for (i = 0; i < ich; ++i) { /* Preset some stuff for main loop */
		if (!oldrp[i])
			oldrp[i] = "";
		childsym[i] = symbol(child(n, i+1));
	}
	Assert(ep->s2 + 1 < sizeof buf);
	strcpy(buf, oldrp[ich]);
	buf[ep->s2] = alt_c ? alt_c : **pstr;
	buf[ep->s2 + 1] = 0;
	pa = parent(ep->focus);
	sympa = pa ? symbol(tree(pa)) : Rootsymbol;
	ci = table[sympa].r_class[ichild(ep->focus) - 1];
	code = Code(oldrp[0][0]);

	for (cp = ci->c_insert; *cp; cp += 2) {
		if (cp[0] != code)
			continue;
		sym = cp[1];
		if (sym >= TABLEN)
			continue;
		if (sym == oldsym) {
			anyok = Yes;
			continue;
		}
		tp = &table[sym];
		newrp = tp->r_repr;
		ok = Yes;
		for (i = 0; i < ich; ++i) {
			str = newrp[i];
			if (!str)
				str = "";
			if (!Strequ(str, oldrp[i])
				|| childsym[i] != Optional && childsym[i] != Hole
					&& !isinclass(childsym[i], tp->r_class[i])) {
				ok = No;
				break;
			}
		}
		if (!ok)
			continue;
		str = newrp[i];
		if (!str || !Strnequ(str, buf, ep->s2+1))
			continue;
		if (anyok) {
			if (Strequ(str, oldrp[ich]))
				continue; /* Same as it was: no new suggestion */
			symfound = sym;
			break;
		}
		else if (symfound < 0 && !Strequ(str, oldrp[ich]))
			symfound = sym;
	}

	if (symfound < 0)
		return ifmatch(ep, pstr, oldrp[ich], alt_c);
	nn = table[symfound].r_node;
	for (i = 1; i <= ich; ++i) { /* Copy children to the left of the focus */
		sym = symbol(child(n, i));
		if (sym == Optional || sym == Hole)
			continue;
		setchild(&nn, i, nodecopy(child(n, i)));
	}
	replace(&ep->focus, nn);
	str = newrp[ich];
	do { /* Find easy continuation */
		++ep->s2;
		++*pstr;
	} while (**pstr && **pstr == str[ep->s2]);
	return Yes;
}


/*
 * Refinement for resuggest(): see if there is a match, and if so, find
 * longest match.
 */

Hidden bool
ifmatch(ep, pstr, str, alt_c)
	register environ *ep;
	register string *pstr;
	register string str;
	register int alt_c;
{
	register int c = str[ep->s2];

	if (c != **pstr && (!alt_c || c != alt_c))
		return No;
	do {
		++ep->s2;
		++*pstr;
	} while (**pstr && **pstr == str[ep->s2]);
	return Yes;
}
