/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
static char rcsid[]= "$Header: mkboot.c,v 1.1 85/08/22 15:44:31 timo Exp $";

/*
 * B editor -- Program to create the "boot.h" file (the grammar tables).
 */

#include "b.h"
#include "node.h"
#include "gram.h"
#include "tabl.h"

#include <ctype.h>


/*
 * Test whether sym is in the given class.
 */

Visible bool
isinclass(sym, ci)
	int sym;
	struct classinfo *ci;
{
	classptr cp;

	Assert(ci && ci->c_class);
	if (sym == Hole)
		return !isinclass(Optional, ci);
	for (cp = ci->c_class; *cp; ++cp)
		if (sym == *cp)
			return Yes;
	return No;
}


main()
{
	int sym;
	int nch;
	struct classinfo **cp;
	struct classinfo *sp;

	printf("/* boot.h -- data file for grammar tables. */\n\n");

	/* Check the representations.
	   The code assumes Optional and Hole are the last symbols
	   in the table, i.e. the first processed by the loop. */

	for (sym = TABLEN-1; sym >= 0; --sym) {
		if (table[sym].r_symbol != sym) {
			if (sym != Hole && sym != Optional
					&& table[sym].r_symbol == 0)
				continue; /* Disabled table entry */
			syserr("initgram: table order (%s=%d, should be %d)",
				table[sym].r_name, table[sym].r_symbol, sym);
		}
		cp = table[sym].r_class;
		for (nch = 0; nch < MAXCHILD && (sp = cp[nch]); ++nch)
			;
		ckrepr(table[sym].r_repr, nch);
	}

	initcodes();
	initclasses();
	dumptable();
	exit(0);
}


/*
 * Check a representation array (subroutine for initgram).
 */

Hidden Procedure
ckrepr(rp, nch)
	string *rp;
	int nch;
{
	string cp;
	int i;
	int ich;

	for (ich = 0; ich <= nch; ++ich) {
		cp = rp[ich];
		if (!cp)
			continue;
		for (i = 0; cp[i]; ++i) {
			switch (cp[i]) {
			case '\n':
			case '\r':
				if (i || ich)
					syserr("initgram (ckrepr): badly placed \\n/\\r");
				break;
			case '\t':
			case '\b':
				if (cp[i+1])
					syserr("initgram (ckrepr): badly placed \\t/\\b");
				break;
			default:
				if (cp[i] < ' ' || cp[i] >= 0177)
					syserr("initgram (ckrepr): illegal control char");
			}
		}
	}
}


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
	int c;

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
 * Initialization routine for the 'struct classinfo' stuff.
 *
 * "Suggestion" is skipped:
 * what can be inserted there is not computed from this table.
 */

Hidden Procedure
initclasses()
{
	int i;
	int j;
	struct table *tp;

	for (i = 0; i < TABLEN; ++i) {
		tp = &table[i];
		if (tp->r_symbol != i || i == Suggestion)
			continue; /* Dead entry */
		for (j = 0; j < MAXCHILD && tp->r_class[j]; ++j) {
			if (!tp->r_class[j]->c_insert)
				defclass(tp->r_class[j]);
		}
	}
}

classptr makelist(); /* Forward */

Hidden Procedure
defclass(p)
	struct classinfo *p;
{
	int c;
	struct table *tp;
	classptr cp;
	classptr cp1;
	classelem insert[1024];
	classelem append[1024];
	classelem join[1024];
	int inslen = 0;
	int applen = 0;
	int joinlen = 0;
	string *rp;
	int fw1;

	cp = p->c_class;
	Assert(cp);

	for (; *cp; ++cp) {
		if (*cp == Optional)
			continue;
		if (*cp >= TABLEN) { /* Insert direct lexical item */
			for (c = 1; c <= lastcode; ++c) {
				if (maystart(Invcode(c), *cp)) {
					Assert(inslen+3 < sizeof insert / sizeof insert[0]);
					insert[inslen] = c;
					insert[inslen+1] = *cp;
					inslen += 2;
				}
			}
			continue;
		}
		tp = &table[*cp];
		rp = tp->r_repr;
		if (!Fw_zero(rp[0])) { /* Insert fixed text */
			c = Code(rp[0][0]);
			Assert(inslen+3 < sizeof insert / sizeof insert[0]);
			insert[inslen] = c;
			insert[inslen+1] = *cp;
			inslen += 2;
			continue;
		}
		Assert(tp->r_class[0]);
		cp1 = tp->r_class[0]->c_class;
		Assert(cp1);
		for (; *cp1; ++cp1) {
			if (*cp1 < TABLEN)
				continue;
			for (c = 1; c <= lastcode; ++c) { /* Insert indir. lex. items */
				if (maystart(Invcode(c), *cp1)) {
					Assert(inslen+3 < sizeof insert / sizeof insert[0]);
					insert[inslen] = c;
					insert[inslen+1] = *cp;
					inslen += 2;
				}
			}
		}
		fw1 = Fwidth(rp[1]);
		if (fw1) { /* Append */
			c = rp[1][0];
			Assert(c > 0 && c < RANGE);
			if (c == ' ') {
				c = rp[1][1];
				if (!c || c == '\b' || c == '\t')
					c = ' ';
				else
					c |= 0200;
			}
			Assert(applen+3 < sizeof append / sizeof append[0]);
			append[applen] = c;
			append[applen+1] = *cp;
			applen += 2;
		}
		if ((!fw1 || fw1 == 1 && rp[1][0] == ' ')
			&& tp->r_class[1]) { /* Join */
			Assert(joinlen+3 < sizeof join / sizeof join[0]);
			join[joinlen] = 1 + fw1;
			join[joinlen+1] = *cp;
			joinlen += 2;
		}
	}

	Assert(inslen); /* Dead alley */
	insert[inslen] = 0;
	p->c_insert = makelist(insert, inslen + 1);
	if (applen) {
		append[applen] = 0;
		p->c_append = makelist(append, applen + 1);
	}
	if (joinlen) {
		join[joinlen] = 0;
		p->c_join = makelist(join, joinlen + 1);
	}
}

Hidden classptr
makelist(list, len)
	classptr list;
	int len;
{
	classptr cp =
		(classptr) malloc((unsigned) (len*sizeof(classelem)));
	int i;

	if (!cp)
		syserr("makelist: malloc");
	for (i = 0; i < len; ++i, ++list)
		cp[i] = *list;
#ifndef NDEBUG
	if (index(cp, '\0') != cp+len-1)
		printf("makelist: zero in string!\n");
#endif
	return cp;
}

#define MAXLOOKUP 1000

Hidden struct classinfo **known;
Hidden int nknown;

Hidden Procedure
dumptable()
{
	int sym;

	getclassinfos();
	printf("Hidden struct table b_grammar[%d] = {\n", TABLEN);
	for (sym= 0; sym < TABLEN; ++sym)
		dumpentry(table+sym);
	printf("};\n");
	free(known);
}

Hidden Procedure
getclassinfos()
{
	int sym, k;

	known= (struct classinfo **) malloc(MAXLOOKUP * sizeof(struct classinfo*));
	if (known == NULL)
		syserr("getclassinfos: can't malloc 'known' array");
	nknown= 0;
	printf("Hidden struct classinfo cl[] = {\n");
	for (sym= 0; sym < TABLEN; ++sym) {
		for (k= 0; k < MAXCHILD; ++k)
			lookup(table[sym].r_class[k]);
	}
	printf("};\n\n");
}

Hidden int
lookup(ci)
	struct classinfo *ci;
{
	int k;

	if (ci == NULL)
		return -1;
	for (k= 0; k < nknown; ++k) {
		if (known[k] == ci)
			return k;
	}
	if (k < MAXLOOKUP) {
		++nknown;
		known[k]= ci;
		printf("/*%d*/", k);
		dumpclassinfo(ci);
	}
}

Hidden Procedure
dumpclassinfo(ci)
	struct classinfo *ci;
{
	printf("\t{");
	dumpstring(ci->c_class);
	printf("\n\t");
	dumpstring(ci->c_insert);
	printf("\n\t");
	dumpstring(ci->c_append);
	printf("\n\t");
	dumpstring(ci->c_join);
	printf("},\n");
}

Hidden Procedure
dumpentry(p)
	struct table *p;
{
	int k;

	printf("\t{%2d, ", p->r_symbol);
	dumpstring(p->r_name);
	printf(" {");
	for (k= 0; k <= MAXCHILD; ++k)
		dumpstring(p->r_repr[k]);
	printf("}, {");
	for (k= 0; k < MAXCHILD; ++k)
		refclassinfo(p->r_class[k]);
	printf("}, 0},\n");
}

Hidden Procedure
dumpstring(s)
	string s;
{
	char c;

	if (s == NULL) {
		printf("0, ");
		return;
	}
	printf("\"");
	for (; (c= *s) != '\0'; ++s) {
		if (c >= ' ' && c < 0177) {
			if (c == '\\' || c == '"')
				printf("\\");
			printf("%c", c);
		}
		else if (c == '\t')
			printf("\\t");
		else if (c == '\b')
			printf("\\b");
		else
			printf("\\%03o", c&0377);
	}
	printf("\", ");
}

Hidden Procedure
refclassinfo(ci)
	struct classinfo ci;
{
	int k= lookup(ci);

	if (k >= 0)
		printf("&cl[%d], ", k);
	else
		printf("0, ");
}


/*
 * Yield the width of a piece of fixed text as found in a node's repr,
 * excluding \b or \t.  If \n or \r is found, -1 is returned.
 * It assumes that \n or \r only occur as first
 * character, and \b or \t only as last.
 */

Visible int
fwidth(str)
	register string str;
{
	register int c;
	register int n = 0;

	if (!str)
		return 0;
	c = str[0];
	if (c == '\r' || c == '\n')
		return -1;
	for (; c; c = *++str)
		++n;
	if (n > 0) {
		c = str[-1];
		if (c == '\t' || c == '\b')
			--n;
	}
	return n;
}


Visible Procedure
syserr(fmt, a1, a2, a3, a4, a5)
	string fmt;
{
	fprintf(stderr, "mkboot system error:\n");
	fprintf(stderr, fmt, a1, a2, a3, a4, a5);
	fprintf(stderr, "\n");
	exit(1);
}


Visible Procedure
asserr(file, line)
	string file;
	int line;
{
	syserr("assertion error: %s, line %d", file, line);
}
