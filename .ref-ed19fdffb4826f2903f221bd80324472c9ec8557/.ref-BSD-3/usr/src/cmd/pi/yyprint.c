/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.1 February 1978
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.1 February 1978
 */

#include "whoami"
#include "0.h"
#include "yy.h"

char	*tokname();

STATIC	bool bounce;

/*
 * Printing representation of a
 * "character" - a lexical token
 * not in a yytok structure.
 * 'which' indicates which char * you want
 * should always be called as "charname(...,0),charname(...,1)"
 */
char *
charname(ch , which )
	int ch;
	int which;
{
	struct yytok Ych;

	Ych.Yychar = ch;
	Ych.Yylval = nullsem(ch);
	return (tokname(&Ych , which ));
}

/*
 * Printing representation of a token
 * 'which' as above.
 */
char *
tokname(tp , which )
	register struct yytok *tp;
	int	 	      which;
{
	register char *cp;
	register struct kwtab *kp;
	char	*cp2;

	cp2 = "";
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
				cp = "'x'\0'x'\0'x'\0'x'";
				/*
				 * for four times reentrant code!
				 * used to be:
				 * if (bounce = ((bounce + 1) & 1))
				 *	cp += 4;
				 */
				bounce = ( bounce + 1 ) % 4;
				cp += (4 * bounce);	/* 'x'\0 is 4 chars */
				cp[1] = tp->Yychar;
				break;
			}
			for (kp = yykey; kp->kw_str != NIL && kp->kw_val != tp->Yychar; kp++)
				continue;
			cp = "keyword ";
			cp2 = kp->kw_str;
	}
	return ( which ? cp2 : cp );
}
