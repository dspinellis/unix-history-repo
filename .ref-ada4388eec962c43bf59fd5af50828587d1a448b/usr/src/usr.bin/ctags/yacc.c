/*
 * Copyright (c) 1987, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)yacc.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

#include "ctags.h"

/*
 * y_entries:
 *	find the yacc tags and put them in.
 */
void
y_entries()
{
	int	c;
	char	*sp;
	bool	in_rule;
	char	tok[MAXTOKEN];

	in_rule = NO;

	while (GETC(!=, EOF))
		switch (c) {
		case '\n':
			SETLINE;
			/* FALLTHROUGH */
		case ' ':
		case '\f':
		case '\r':
		case '\t':
			break;
		case '{':
			if (skip_key('}'))
				in_rule = NO;
			break;
		case '\'':
		case '"':
			if (skip_key(c))
				in_rule = NO;
			break;
		case '%':
			if (GETC(==, '%'))
				return;
			(void)ungetc(c, inf);
			break;
		case '/':
			if (GETC(==, '*'))
				skip_comment();
			else
				(void)ungetc(c, inf);
			break;
		case '|':
		case ';':
			in_rule = NO;
			break;
		default:
			if (in_rule || !isalpha(c) && c != '.' && c != '_')
				break;
			sp = tok;
			*sp++ = c;
			while (GETC(!=, EOF) && (intoken(c) || c == '.'))
				*sp++ = c;
			*sp = EOS;
			getline();		/* may change before ':' */
			while (iswhite(c)) {
				if (c == '\n')
					SETLINE;
				if (GETC(==, EOF))
					return;
			}
			if (c == ':') {
				pfnote(tok, lineno);
				in_rule = YES;
			}
			else
				(void)ungetc(c, inf);
		}
}

/*
 * toss_yysec --
 *	throw away lines up to the next "\n%%\n"
 */
void
toss_yysec()
{
	int	c;			/* read character */
	int	state;

	/*
	 * state == 0 : waiting
	 * state == 1 : received a newline
	 * state == 2 : received first %
	 * state == 3 : recieved second %
	 */
	lineftell = ftell(inf);
	for (state = 0; GETC(!=, EOF);)
		switch (c) {
		case '\n':
			++lineno;
			lineftell = ftell(inf);
			if (state == 3)		/* done! */
				return;
			state = 1;		/* start over */
			break;
		case '%':
			if (state)		/* if 1 or 2 */
				++state;	/* goto 3 */
			break;
		default:
			state = 0;		/* reset */
			break;
		}
}
