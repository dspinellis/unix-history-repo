%{
/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)arith_lex.l	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <unistd.h>
#include "y.tab.h"
#include "error.h"

extern yylval;
extern char *arith_buf, *arith_startbuf;
#undef YY_INPUT
#define YY_INPUT(buf,result,max) \
	result = (*buf = *arith_buf++) ? 1 : YY_NULL;
%}

%%
[ \t\n]	{ ; }
[0-9]+	{ yylval = atol(yytext); return(ARITH_NUM); }
"("	{ return(ARITH_LPAREN); }
")"	{ return(ARITH_RPAREN); }
"||"	{ return(ARITH_OR); }
"&&"	{ return(ARITH_AND); }
"|"	{ return(ARITH_BOR); }
"^"	{ return(ARITH_BXOR); }
"&"	{ return(ARITH_BAND); }
"=="	{ return(ARITH_EQ); }
"!="	{ return(ARITH_NE); }
">"	{ return(ARITH_GT); }
">="	{ return(ARITH_GE); }
"<"	{ return(ARITH_LT); }
"<="	{ return(ARITH_LE); }
"<<"	{ return(ARITH_LSHIFT); }
">>"	{ return(ARITH_RSHIFT); }
"*"	{ return(ARITH_MUL); }
"/"	{ return(ARITH_DIV); }
"%"	{ return(ARITH_REM); }
"+"	{ return(ARITH_ADD); } 
"-"	{ return(ARITH_SUB); }
"~"	{ return(ARITH_BNOT); }
"!"	{ return(ARITH_NOT); }
.	{ error("arith: syntax error: \"%s\"\n", arith_startbuf); }
%%

void
arith_lex_reset() {
	YY_NEW_FILE;
}
