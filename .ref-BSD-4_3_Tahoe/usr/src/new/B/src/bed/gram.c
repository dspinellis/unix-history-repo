/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
static char rcsid[] = "$Header: gram.c,v 2.5 85/08/22 16:03:16 timo Exp $";

/*
 * B editor -- All routines referencing the grammar table are in this file.
 */

#include "b.h"
#include "feat.h"
#include "bobj.h"
#include "node.h"
#include "gram.h"
#include "supr.h"
#include "tabl.h"

extern bool dflag;

#include <ctype.h>


/*
 * Test whether sym is in the given class.
 */

Visible bool
isinclass(sym, ci)
	register int sym;
	struct classinfo *ci;
{
	register classptr cp;

	Assert(ci && ci->c_class);
	if (sym == Hole)
		return !isinclass(Optional, ci);
	for (cp = ci->c_class; *cp; ++cp)
		if (sym == *cp)
			return Yes;
	return No;
}


/*
 * Deliver the representation array for the given node.
 * If the node is actually just a "text" value, construct
 * one in static storage -- which is overwritten at each call.
 * In this case there are two deficiencies: the next call to
 * noderepr which uses the same feature overwrites the reply
 * value of the previous call, AND if the text value itself
 * is changed, the representation may change, too.
 * In practical use this is no problem at all, however.
 */

Visible string *
noderepr(n)
	register node n;
{
	register int sym;

	if (n && Type(n) == Tex) {
		static string buf[2];
		buf[0] = Str((value)n);
		return buf;
	}
	sym = symbol(n);
	return table[sym].r_repr;
}


/*
 * Deliver the prototype node for the given symbol.
 */

Visible node
gram(sym)
	register int sym;
{
	Assert(sym == 0 || sym > 0 && sym < TABLEN && table[sym].r_symbol);
	return table[sym].r_node;
}

#ifdef SAVEBUF

/*
 * Deliver the name of a symbol.
 */

Visible string
symname(sym)
	int sym;
{
	static char buf[20];

	if (sym >= 0 && sym < TABLEN && table[sym].r_name)
		return table[sym].r_name;
	sprintf(buf, "%d", sym);
	return buf;
}


/*
 * Find the symbol corresponding to a given name.
 * Return -1 if not found.
 */

Visible int
nametosym(str)
	register string str;
{
	register int sym;
	register string name;

	for (sym = 0; sym < TABLEN; ++sym) {
		name = table[sym].r_name;
		if (name && Strequ(name, str))
			return sym;
	}
	return -1;
}

#endif SAVEBUF

/*
 * Test whether `sym' may replace the node in the path `p'.
 */

Visible bool
allowed(p, sym)
	register path p;
	register int sym;
{
	register path pa = parent(p);
	register int ich = ichild(p);
	register int sympa = pa ? symbol(tree(pa)) : Rootsymbol;

	Assert(sympa >= 0 && sympa < TABLEN && ich > 0 && ich <= MAXCHILD);
	return isinclass(sym, table[sympa].r_class[ich-1]);
}


/*
 * Initialize (and verify) the grammar table.
 */

Visible Procedure
initgram()
{
	register int sym;
	register int nch;
	register struct classinfo **cp;
	register struct classinfo *sp;
	node ch[MAXCHILD];

#ifndef NDEBUG
	if (dflag)
		fprintf(stderr, "*** initgram();\n\r");
#endif NDEBUG
	/* Set the node pointers in the table and check the representations.
	   The code assumes Optional and Hole are the last
	   symbols in the table, i.e. the first processed by the loop. */

	for (sym = TABLEN-1; sym >= 0; --sym) {
		if (table[sym].r_symbol != sym) {
			if (sym != Hole && sym != Optional && table[sym].r_symbol == 0)
				continue; /* Disabled table entry */
			syserr("initgram: table order (%s=%d, should be %d)",
				table[sym].r_name, table[sym].r_symbol, sym);
		}
		cp = table[sym].r_class;
		for (nch = 0; nch < MAXCHILD && (sp = cp[nch]); ++nch)
			ch[nch] =
				table[sp->c_class[0] == Optional ?
					Optional : Hole].r_node;
		table[sym].r_node = newnode(nch, sym, ch);
		fix((value) table[sym].r_node);
	}

	initcodes();
#ifdef USERSUGG
	initclasses();
#endif USERSUGG
}


#ifdef USERSUGG

/*
 * Add built-in commands to the suggestion tables.
 */

Hidden Procedure
initclasses()
{
	register int i;
	register int j;
	register struct table *tp;

	for (i = 0; i < TABLEN; ++i) {
		tp = &table[i];
		if (tp->r_symbol != i || i == Suggestion)
			continue; /* Dead entry */
		for (j = 0; j < MAXCHILD && tp->r_class[j]; ++j) {
			if (isinclass(Suggestion, tp->r_class[j]))
				makesugg(tp->r_class[j]->c_class);
		}
	}
}


/*
 * Extract suggestions from class list.
 */

Hidden Procedure
makesugg(cp)
	classptr cp;
{
	struct table *tp;
	string *rp;
	char buffer[1000];
	string bp;
	string sp;
	int i;
	int nch;

	for (; *cp; ++cp) {
		if (*cp >= TABLEN || *cp < 0)
			continue;
		tp = &table[*cp];
		rp = tp->r_repr;
		if (rp[0] && isupper(rp[0][0])) {
			bp = buffer;
			nch = nchildren(tp->r_node);
			for (i = 0; i <= nch; ++i) {
				if (rp[i]) {
					for (sp = rp[i]; *sp >= ' '; ++sp)
						*bp++ = *sp;
				}
				if (i < nch && !isinclass(Optional, tp->r_class[i]))
					*bp++ = '?';
			}
			if (bp > buffer) {
				*bp = 0;
				addsugg(buffer, Yes);
			}
		}
	}
}

#endif USERSUGG


/*
 * Compaction scheme for characters to save space in grammar tables
 * by combining characters with similar properties (digits, l.c. letters).
 */

#define RANGE 128 /* ASCII characters are in {0 .. RANGE-1} */

Visible char code_array[RANGE];
Visible char invcode_array[RANGE];
Visible int lastcode;

Hidden Procedure
initcodes()
{
	register int c;

	code_array['\n'] = ++lastcode;
	invcode_array[lastcode] = '\n';
	for (c = ' '; c <= '0'; ++c) {
		code_array[c] = ++lastcode;
		invcode_array[lastcode] = c;
	}
	for (; c <= '9'; ++c)
		code_array[c] = lastcode;
	for (; c <= 'a'; ++c) {
		code_array[c] = ++lastcode;
		invcode_array[lastcode] = c;
	}
	for (; c <= 'z'; ++c)
		code_array[c] = lastcode;
	for (; c < RANGE; ++c) {
		code_array[c] = ++lastcode;
		invcode_array[lastcode] = c;
	}
}


/*
 * Set the root of the grammar to the given symbol.  It must exist.
 */

Visible Procedure
setroot(name)
	string name;
{
	register int k;
	register int i;

	for (k = 1; k < TABLEN; ++k) {
		if (table[k].r_name && Strequ(name, table[k].r_name)) {
			table[Rootsymbol].r_symbol = table[k].r_symbol;
			table[Rootsymbol].r_name = table[k].r_name;
			for (i = 0; i < MAXCHILD; ++i) {
				table[Rootsymbol].r_repr[i] = table[k].r_repr[i];
				table[Rootsymbol].r_class[i] = table[k].r_class[i];
			}
			table[Rootsymbol].r_repr[i] = table[k].r_repr[i];
			table[Rootsymbol].r_node = table[k].r_node;
			table[Rootsymbol].r_symbol = Rootsymbol;
			return;
		}
	}
	syserr("Can't set root of grammar to <%s>", name);
}


/*
 * The remainder of this file is specific for the currently used grammar.
 */


#include "boot.h" /* Has static data, so should be included only once! */
#include "syms.h"

Visible struct table *table = b_grammar;


/*
 * Table indicating which symbols are used to form lists of items.
 * Consulted via predicate 'issublist' in "gram.c".
 */

Hidden classelem Asublists[] = {
	E_plus, F_e_plus, 
	And, And_kw, Or, Or_kw,
	0,
};

Hidden struct classinfo sublists[] = {Asublists};


/*
 * Predicate telling whether two symbols can form lists together.
 * This is important for list whose elements must alternate in some
 * way, as is the case for [KEYWORD [expression] ]*.
 *
 * This code must be in this file, otherwise the names and values
 * of the symbols would have to be made public.
 */

Visible bool
samelevel(sym, sym1)
	register int sym;
	register int sym1;
{
	register int zzz;

	if (sym1 == sym)
		return Yes;
	if (sym1 < sym)
		zzz = sym, sym = sym1, sym1 = zzz; /* Ensure sym <= sym1 */
	/* Now always sym < sym1 */
	return sym == Kw_plus && sym1 == E_plus
		|| sym == F_kw_plus && sym1 == F_e_plus
		|| sym == And && sym1 == And_kw
		|| sym == Or && sym1 == Or_kw;
}


/*
 * Predicate to tell whether a symbol can form chained lists.
 * By definition, all right-recursive symbols can do so;
 * in addition, those listed in the class 'sublists' can do
 * it, too (this is used for lists formed of alternating members
 * such as KW expr KW ...).
 */

Visible bool
issublist(sym)
	register int sym;
{
	register int i;
	register string repr;

	Assert(sym < TABLEN);
	if (isinclass(sym, sublists))
		return Yes;
	repr = table[sym].r_repr[0];
	if (Fw_positive(repr))
		return No;
	for (i = 0; i < MAXCHILD && table[sym].r_class[i]; ++i)
		;
	if (i <= 0)
		return No;
	repr = table[sym].r_repr[i];
	if (!Fw_zero(repr))
		return No;
	return isinclass(sym, table[sym].r_class[i-1]);
}
