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
static char sccsid[] = "@(#)arith_lex.l	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "y.tab.h"

extern yylval;
extern char *arith_buf, *arith_startbuf;
int arith_wasoper;
#undef YY_INPUT
#define YY_INPUT(buf,result,max) \
	result = (*buf = *arith_buf++) ? 1 : YY_NULL;
%}

%%
[ \t\n]	{ ; }
[0-9]+	{ arith_wasoper = 0; yylval = atol(yytext); return(ARITH_NUM); }
"("	{ arith_wasoper = 1; return(ARITH_LPAREN); }
")"	{ arith_wasoper = 0; return(ARITH_RPAREN); }
"||"	{ arith_wasoper = 1; return(ARITH_OR); }
"&&"	{ arith_wasoper = 1; return(ARITH_AND); }
"=="	{ arith_wasoper = 1; return(ARITH_EQ); }
">"	{ arith_wasoper = 1; return(ARITH_GT); }
">="	{ arith_wasoper = 1; return(ARITH_GEQ); }
"<"	{ arith_wasoper = 1; return(ARITH_LT); }
"<="	{ arith_wasoper = 1; return(ARITH_LEQ); }
"!="	{ arith_wasoper = 1; return(ARITH_NEQ); }
"*"	{ arith_wasoper = 1; return(ARITH_MULT); }
"/"	{ arith_wasoper = 1; return(ARITH_DIV); }
"%"	{ arith_wasoper = 1; return(ARITH_REM); }
"+"	{ if (!arith_wasoper) {	/* ignore unary plus */
		arith_wasoper = 1; 
		return(ARITH_ADD);
	 } 
	}	
"-"	{ if (arith_wasoper) {
		return(ARITH_UNARYMINUS);
	  } else {
		arith_wasoper = 1;
		return(ARITH_SUBT);
	  }
	}
"!"	{ arith_wasoper = 1; return(ARITH_NOT); }
.	{ error("arith: syntax error: \"%s\"\n", arith_startbuf); }
%%

arith_lex_reset() {
	YY_NEW_FILE;
}
