/*
 * Copyright (c) 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)yacc.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <ctags.h>
#include <string.h>

/*
 * y_entries:
 *	find the yacc tags and put them in.
 */
y_entries()
{
	register int	c;
	register char	*sp;
	register bool	in_rule;
	char	tok[MAXTOKEN];

	while (GETC(!=,EOF))
		switch ((char)c) {
		case '\n':
			SETLINE;
			/* FALLTHROUGH */
		case ' ':
		case '\f':
		case '\r':
		case '\t':
			break;
		case '{':
			if (skip_key((int)'}'))
				in_rule = NO;
			break;
		case '\'':
		case '"':
			if (skip_key(c))
				in_rule = NO;
			break;
		case '%':
			if (GETC(==,'%'))
				return;
			(void)ungetc(c,inf);
			break;
		case '/':
			if (GETC(==,'*'))
				skip_comment();
			else
				(void)ungetc(c,inf);
			break;
		case '|':
		case ';':
			in_rule = NO;
			break;
		default:
			if (in_rule || !isalpha(c) && c != (int)'.'
			    && c != (int)'_')
				break;
			sp = tok;
			*sp++ = c;
			while (GETC(!=,EOF) && (intoken(c) || c == (int)'.'))
				*sp++ = c;
			*sp = EOS;
			getline();		/* may change before ':' */
			while (iswhite(c)) {
				if (c == (int)'\n')
					SETLINE;
				if (GETC(==,EOF))
					return;
			}
			if (c == (int)':') {
				pfnote(tok,lineno);
				in_rule = YES;
			}
			else
				(void)ungetc(c,inf);
		}
}

/*
 * toss_yysec --
 *	throw away lines up to the next "\n%%\n"
 */
toss_yysec()
{
	register int	c,			/* read character */
			state;

	/*
	 * state == 0 : waiting
	 * state == 1 : received a newline
	 * state == 2 : received first %
	 * state == 3 : recieved second %
	 */
	lineftell = ftell(inf);
	for (state = 0;GETC(!=,EOF);)
		switch ((char)c) {
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
		}
}
