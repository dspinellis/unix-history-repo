/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)yylex.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree_ty.h"	/* must be included for yy.h */
#include "yy.h"

/*
 * Scanner
 */
int	yylacnt;

#define	YYLASIZ	10

struct	yytok Yla[YYLASIZ];

unyylex(y)
	struct yytok *y;
{

	if (yylacnt == YYLASIZ)
		panic("unyylex");
	copy((char *) &Yla[yylacnt], (char *) y, sizeof Yla[0]);
	yylacnt++;

}

yylex()
{
	register c;
	register int **ip;
	register char *cp;
	int f;
	char delim;

	if (yylacnt != 0) {
		yylacnt--;
		copy((char *) &Y, (char *) &Yla[yylacnt], sizeof Y);
		return (yychar);
	}
	if (c = yysavc)
		yysavc = 0;
	else
		c = readch();
#ifdef PXP
	yytokcnt++;
#endif

next:
	/*
	 * skip white space
	 */
#ifdef PXP
	yywhcnt = 0;
#endif
	while (c == ' ' || c == '\t') {
#ifdef PXP
		if (c == '\t')
			yywhcnt++;
		yywhcnt++;
#endif
		c = readch();
	}
	yyecol = yycol;
	yyeline = yyline;
	yyefile = filename;
	yyeseqid = yyseqid;
	yyseekp = yylinpt;
	cp = token;
	yylval = yyline;
	switch (c) {
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': 
		case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': 
		case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': 
		case 'v': case 'w': case 'x': case 'y': case 'z': 
		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': 
		case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': 
		case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': 
		case 'V': case 'W': case 'X': case 'Y': case 'Z': 
			do {
				*cp++ = c;
				c = readch();
			} while (alph(c) || digit(c));
			*cp = 0;
			if (opt('s'))
				for (cp = token; *cp; cp++)
					if (*cp >= 'A' && *cp <= 'Z') {
						*cp |= ' ';
					}
			yysavc = c;
			ip = (int **) hash((char *) 0, 1);
			if (*ip < (int *) yykey || *ip >= (int *) lastkey) {
				yylval = (int) *ip;
				return (YID);
			}
			yylval = yyline;
			/*
			 * For keywords
			 * the lexical token
			 * is magically retrieved
			 * from the keyword table.
			 */
			return ((*ip)[1]);
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			f = 0;
			do {
				*cp++ = c;
				c = readch();
			} while (digit(c));
			if (c == 'b' || c == 'B') {
				/*
				 * nonstandard - octal constants
				 */
				if (opt('s')) {
					standard();
					yerror("Octal constants are non-standard");
				}
				*cp = 0;
				yylval = copystr(token);
				return (YBINT);
			}
			if (c == '.') {
				c = readch();
				if (c == '.') {
					*cp = 0;
					yysavc = YDOTDOT;
					yylval = copystr(token);
					return (YINT);
				}
infpnumb:
				f++;
				*cp++ = '.';
				if (!digit(c)) {
					yyset();
					recovered();
					yerror("Digits required after decimal point");
					*cp++ = '0';
				} else
					while (digit(c)) {
						*cp++ = c;
						c = readch();
					}
			}
			if (c == 'e' || c == 'E') {
				f++;
				*cp++ = c;
				if ((c = yysavc) == 0)
					c = readch();
				if (c == '+' || c == '-') {
					*cp++ = c;
					c = readch();
				}
				if (!digit(c)) {
					yyset();
					yerror("Digits required in exponent");
					*cp++ = '0';
				} else
					while (digit(c)) {
						*cp++ = c;
						c = readch();
					}
			}
			*cp = 0;
			yysavc = c;
			yylval = copystr(token);
			if (f)
				return (YNUMB);
			return (YINT);
		case '"':
		case '`':
		case '#':
			if (!any(bufp + 1, c))
				goto illch;
			if (!dquote) {
				recovered();
				dquote++;
				yerror("Character/string delimiter is '");
			}
		case '\'':
			delim = c;
			do {
				do {
					c = readch();
					if (c == '\n') {
						yerror("Unmatched %c for string", (char *) delim);
						if (cp == token)
							*cp++ = ' ', cp++;
						break;
					}
					*cp++ = c;
				} while (c != delim);
				c = readch();
			} while (c == delim);
			*--cp = 0;
			if (cp == token) {
				yerror("Null string not allowed");
				*cp++ = ' ';
				*cp++ = 0;
			}
			yysavc = c;
			yylval = copystr(token);
			return (YSTRING);
		case '.':
			c = readch();
			if (c == '.')
				return (YDOTDOT);
			if (digit(c)) {
				recovered();
				yerror("Digits required before decimal point");
				*cp++ = '0';
				goto infpnumb;
			}
			yysavc = c;
			return ('.');
		case '{':
			/*
			 * { ... } comment
			 */
#ifdef PXP
			getcm(c);
#endif
#ifdef PI
			c = options();
			while (c != '}') {
				if (c <= 0)
					goto nonterm;
				if (c == '{') {
					warning();
					yyset();
					yerror("{ in a { ... } comment");
				}
				c = readch();
			}
#endif
			c = readch();
			goto next;
		case '(':
			if ((c = readch()) == '*') {
				/*
				 * (* ... *) comment
				 */
#ifdef PXP
				getcm(c);
				c = readch();
				goto next;
#endif
#ifdef PI
				c = options();
				for (;;) {
					if (c < 0) {
nonterm:
						yerror("Comment does not terminate - QUIT");
						pexit(ERRS);
					}
					if (c == '(' && (c = readch()) == '*') {
						warning();
						yyset();
						yerror("(* in a (* ... *) comment");
					}
					if (c == '*') {
						if ((c = readch()) != ')')
							continue;
						c = readch();
						goto next;
					}
					c = readch();
				}
#endif
			}
			yysavc = c;
			c = '(';
		case ';':
		case ',':
		case ':':
		case '=':
		case '*':
		case '+':
		case '/':
		case '-':
		case ')':
		case '[':
		case ']':
		case '<':
		case '>':
		case '^':
			return (c);
		case '~':
		case '|':
		case '&':
			if ( opt('s') ) {
			    yyset();
			    standard();
			    yerror("%c is non-standard", (char *) c);
			}
			return c;
		default:
			switch (c) {
				case YDOTDOT:
					return (c);
				case '\n':
					c = readch();
#ifdef PXP
					yytokcnt++;
#endif
					goto next;
				case '\f':
					c = readch();
					goto next;
			}
			if (c <= 0)
				return (YEOF);
illch:
			do
				yysavc = readch();
			while (yysavc == c);
			yylval = c;
			return (YILLCH);
	}
}

yyset()
{

	yyecol = yycol;
	yyeline = yyline;
	yyefile = filename;
	yyseekp = yylinpt;
}

/*
 * Setuflg trims the current
 * input line to at most 72 chars
 * for the u option.
 */
setuflg()
{

	if (charbuf[71] != '\n') {
		charbuf[72] = '\n';
		charbuf[73] = 0;
	}
}
