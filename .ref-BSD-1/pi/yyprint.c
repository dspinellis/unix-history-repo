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

long	tokname();

STATIC	char bounce;

/*
 * Printing representation of a
 * "character" - a lexical token
 * not in a yytok structure.
 */
long
charname(ch)
	int ch;
{
	struct yytok Ych;

	Ych.Yychar = ch;
	Ych.Yylval = nullsem(ch);
	return (tokname(&Ych));
}

/*
 * Printing representation of a token
 */
long
tokname(tp)
	register struct yytok *tp;
{
	register char *cp;
	register struct kwtab *kp;
	long l;

	(&l)->pint2 = "";
	switch (tp->Yychar) {
		case YCASELAB:
			cp = "case-label";
			break;
		case YEOF:
			cp = "end-of-file";
			break;
		case YILLCH:
			cp = "illegal character";
			break;
		case 256:
			/* error token */
			cp = "error";
			break;
		case YID:
			cp = "identifier";
			break;
		case YNUMB:
			cp = "real number";
			break;
		case YINT:
		case YBINT:
			cp = "number";
			break;
		case YSTRING:
			cp = tp->Yylval;
			cp = cp == NIL || cp[1] == 0 ? "character" : "string";
			break;
		case YDOTDOT:
			cp = "'..'";
			break;
		default:
			if (tp->Yychar < 256) {
				cp = "'x'\0'x'";
				if (bounce = ((bounce + 1) & 1))
					cp =+ 4;
				cp[1] = tp->Yychar;
				break;
			}
			for (kp = yykey; kp->kw_str != NIL && kp->kw_val != tp->Yychar; kp++)
				continue;
			cp = "keyword ";
			(&l)->pint2 = kp->kw_str;
	}
	(&l)->pint = cp;
	return (l);
}
