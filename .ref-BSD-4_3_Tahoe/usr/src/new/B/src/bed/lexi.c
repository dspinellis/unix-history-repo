/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: lexi.c,v 2.4 84/10/26 12:01:34 guido Exp $";

/*
 * B editor -- Lexical elements (identifiers, keywords, numbers etc.)
 */

#include <ctype.h>

#include "b.h"
#include "bobj.h"
#include "node.h"
#include "gram.h"


/*
 * Table defining lexical elements.
 *
 * ********** Indexed by (symbol-LEXICAL).
 */

Hidden char lowercase[] = "0123456789'\"abcdefghijklmnopqrstuvwxyz";
Hidden char uppercase[] = "0123456789'\"ABCDEFGHIJKLMNOPQRSTUVWXYZ";
Hidden char digits[] = "0123456789";

Hidden struct {
	string l_start;
	string l_continue;
} chclass[] = {
	{lowercase+12, lowercase,}, /* IDENT */
	{uppercase+12, uppercase,}, /* KEYWORD */
	{digits, digits,}, /* NUMBER */
	{"\\", "^",}, /* COMMENT */
	{"^`'", "^`'",}, /* TEXT1 */
	{"^`\"", "^`\"",}, /* TEXT2 */
	{".+-*/#^~@|<=>", "",}, /* OPERATORS */
	{"^", "^",}, /* RAW_INPUT */
	{"", "",}, /* SUGGESTION (dummy) */
};

#define NCHCLASS (sizeof(chclass)/sizeof(chclass[0]))


/*
 * Test whether character `c' may start a lexical element with
 * symbolic name `lex'.
 */

Visible bool
maystart(c, lex)
	char c;
	int lex;
{
	string cp;

	lex -= LEXICAL;
	Assert(lex >= 0);
	if (lex >= NCHCLASS || !isascii(c) || c != ' ' && !isprint(c))
		return No;
	cp = chclass[lex].l_start;
	if (*cp == '^')
		return !index(cp+1, c);
	return index(cp, c) != 0;
}


/*
 * Test whether character `c' may continue a lexical element with
 * symbolic name `lex'.
 */

Visible bool
maycontinue(c, lex)
	char c;
	int lex;
{
	string cp;

	lex -= LEXICAL;
	Assert(lex >= 0);
	if (lex >= NCHCLASS || !isascii(c) || c != ' ' && !isprint(c))
		return No;
	cp = chclass[lex].l_continue;
	if (*cp == '^')
		return !index(cp+1, c);
	return index(cp, c) != 0;
}
