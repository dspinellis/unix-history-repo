#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"
#include "yy.h"

/*
 * Scanner
 */
int	yylacnt;

#define	YYLASIZ	10

struct	yytok Yla[YYLASIZ];

unyylex(y)
	struct yylex *y;
{

	if (yylacnt == YYLASIZ)
		panic("unyylex");
	copy(&Yla[yylacnt], y, sizeof Yla[0]);
	yylacnt++;

}

yylex()
{
	register c;
	register **ip;
	register char *cp;
	int f;
	char delim;

	if (yylacnt != 0) {
		yylacnt--;
		copy(&Y, &Yla[yylacnt], sizeof Y);
		return (yychar);
	}
	if (c = yysavc)
		yysavc = 0;
	else
		c = getchar();
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
		c = getchar();
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
				c = getchar();
			} while (alph(c) || digit(c));
			*cp = 0;
			if (opt('s'))
				for (cp = token; *cp; cp++)
					if (*cp >= 'A' && *cp <= 'Z') {
						*cp =| ' ';
					}
			yysavc = c;
			ip = hash(0, 1);
			if (*ip < yykey || *ip >= lastkey) {
				yylval = *ip;
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
				c = getchar();
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
				c = getchar();
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
						c = getchar();
					}
			}
			if (c == 'e' || c == 'E') {
				f++;
				*cp++ = c;
				if ((c = yysavc) == 0)
					c = getchar();
				if (c == '+' || c == '-') {
					*cp++ = c;
					c = getchar();
				}
				if (!digit(c)) {
					yyset();
					yerror("Digits required in exponent");
					*cp++ = '0';
				} else
					while (digit(c)) {
						*cp++ = c;
						c = getchar();
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
			if (!any(bufp + 1, c))
				goto illch;
			if (!dquote) {
				recovered();
				dquote++;
				yerror("Character/string delimiter is '");
			}
		case '\'':
		case '#':
			delim = c;
			do {
				do {
					c = getchar();
					if (c == '\n') {
						yerror("Unmatched %c for string", delim);
						if (cp == token)
							*cp++ = ' ', cp++;
						break;
					}
					*cp++ = c;
				} while (c != delim);
				c = getchar();
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
			c = getchar();
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
				c = getchar();
			}
#endif
			c = getchar();
			goto next;
		case '(':
			if ((c = getchar()) == '*') {
				/*
				 * (* ... *) comment
				 */
#ifdef PXP
				getcm(c);
				c = getchar();
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
					if (c == '(' && (c = getchar()) == '*') {
						warning();
						yyset();
						yerror("(* in a (* ... *) comment");
					}
					if (c == '*') {
						if ((c = getchar()) != ')')
							continue;
						c = getchar();
						goto next;
					}
					c = getchar();
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
		case '|':
		case '&':
		case ')':
		case '[':
		case ']':
		case '<':
		case '>':
		case '~':
		case '^':
			return (c);
		default:
			switch (c) {
				case YDOTDOT:
					return (c);
				case '\n':
					c = getchar();
#ifdef PXP
					yytokcnt++;
#endif
					goto next;
				case '\f':
					c = getchar();
					goto next;
			}
			if (c <= 0)
				return (YEOF);
illch:
			do
				yysavc = getchar();
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
