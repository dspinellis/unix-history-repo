%{
/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Borman at Krystal Technologies.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lex.l	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "ldef.h"
#include "y.tab.h"
%}

ODIGIT	[0-7]
DIGIT	[0-9]
XDIGIT	[0-9a-fA-F]
W	[\t\n\r ]

%%
\'.\'				{ yylval.rune = yytext[1];
				  return(RUNE); }

'\\a'				{ yylval.rune = '\a';
				  return(RUNE); }
'\\b'				{ yylval.rune = '\b';
				  return(RUNE); }
'\\f'				{ yylval.rune = '\f';
				  return(RUNE); }
'\\n'				{ yylval.rune = '\n';
				  return(RUNE); }
'\\r'				{ yylval.rune = '\r';
				  return(RUNE); }
'\\t'				{ yylval.rune = '\t';
				  return(RUNE); }
'\\v'				{ yylval.rune = '\v';
				  return(RUNE); }

0x{XDIGIT}+			{ yylval.rune = strtol(yytext, 0, 16);
				  return(RUNE); }
0{ODIGIT}+			{ yylval.rune = strtol(yytext, 0, 8);
				  return(RUNE); }
{DIGIT}+			{ yylval.rune = strtol(yytext, 0, 10);
				  return(RUNE); }


MAPLOWER			{ return(MAPLOWER); }
MAPUPPER			{ return(MAPUPPER); }
TODIGIT				{ return(DIGITMAP); }
INVALID				{ return(INVALID); }

ALPHA				{ yylval.i = _A|_R|_G; return(LIST); }
CONTROL				{ yylval.i = _C; return(LIST); }
DIGIT				{ yylval.i = _D|_R|_G; return(LIST); }
GRAPH				{ yylval.i = _G|_R; return(LIST); }
LOWER				{ yylval.i = _L|_R|_G; return(LIST); }
PUNCT				{ yylval.i = _P|_R|_G; return(LIST); }
SPACE				{ yylval.i = _S; return(LIST); }
UPPER				{ yylval.i = _U|_R|_G; return(LIST); }
XDIGIT				{ yylval.i = _X|_R|_G; return(LIST); }
BLANK				{ yylval.i = _B; return(LIST); }
PRINT				{ yylval.i = _R; return(LIST); }
IDEOGRAM			{ yylval.i = _I|_R|_G; return(LIST); }
SPECIAL				{ yylval.i = _T|_R|_G; return(LIST); }
PHONOGRAM			{ yylval.i = _Q|_R|_G; return(LIST); }

VARIABLE[\t ]			{ static char vbuf[1024];
				  char *v = vbuf;
				  while ((*v = input()) && *v != '\n')
					++v;
                                  if (*v) {
					unput(*v);
					*v = 0;
				  }
				  yylval.str = vbuf;
				  return(VARIABLE);
				}

ENCODING			{ return(ENCODING); }

\".*\"				{ char *e = yytext + 1;
				  yylval.str = e;
				  while (*e && *e != '"')
					++e;
				  *e = 0;
				  return(STRING); }

\<|\(|\[			{ return(LBRK); }

\>|\)|\]			{ return(RBRK); }

\-				{ return(THRU); }
\.\.\.				{ return(THRU); }

\:				{ return(':'); }

{W}+				;

^\#.*\n				;
\/\*				{ char lc = 0;
				  do {
				    while ((lc) != '*')
					if ((lc = input()) == 0)
					    break;
				  } while((lc = input()) != '/');
				}

\\$				;
.				{ printf("Lex is skipping '%s'\n", yytext); }
%%

#if	!defined(yywrap)
yywrap()
{
	return(1);
}
#endif
