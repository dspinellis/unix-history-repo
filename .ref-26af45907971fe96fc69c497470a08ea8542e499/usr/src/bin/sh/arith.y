%token ARITH_OR ARITH_AND ARITH_ADD ARITH_SUBT ARITH_MULT ARITH_DIV ARITH_REM ARITH_EQ ARITH_GT ARITH_GEQ ARITH_LT ARITH_LEQ ARITH_NEQ
%token ARITH_NUM ARITH_LPAREN ARITH_RPAREN ARITH_NOT ARITH_UNARYMINUS

%left ARITH_OR
%left ARITH_AND
%left ARITH_EQ ARITH_NEQ
%left ARITH_LT ARITH_GT ARITH_GEQ ARITH_LEQ
%left ARITH_ADD ARITH_SUBT
%left ARITH_MULT ARITH_DIV ARITH_REM
%left ARITH_UNARYMINUS ARITH_NOT
%%

exp:	expr = {
			return ($1);
		}
	;


expr:	ARITH_LPAREN expr ARITH_RPAREN = { $$ = $2; }
	| expr ARITH_OR expr   = { $$ = $1 ? $1 : $3 ? $3 : 0; }
	| expr ARITH_AND expr   = { $$ = $1 ? ( $3 ? $3 : 0 ) : 0; }
	| expr ARITH_EQ expr   = { $$ = $1 == $3; }
	| expr ARITH_GT expr   = { $$ = $1 > $3; }
	| expr ARITH_GEQ expr   = { $$ = $1 >= $3; }
	| expr ARITH_LT expr   = { $$ = $1 < $3; }
	| expr ARITH_LEQ expr   = { $$ = $1 <= $3; }
	| expr ARITH_NEQ expr   = { $$ = $1 != $3; }
	| expr ARITH_ADD expr   = { $$ = $1 + $3; }
	| expr ARITH_SUBT expr   = { $$ = $1 - $3; }
	| expr ARITH_MULT expr   = { $$ = $1 * $3; }
	| expr ARITH_DIV expr   = {
			if ($3 == 0)
				yyerror("division by zero");
			$$ = $1 / $3; 
			}
	| expr ARITH_REM expr   = { $$ = $1 % $3; }
	| ARITH_NOT expr	= { $$ = !($2); }
	| ARITH_UNARYMINUS expr = { $$ = -($2); }
	| ARITH_NUM
	;
%%
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
static char sccsid[] = "@(#)arith.y	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "shell.h"
#include "error.h"
#include "output.h"
#include "memalloc.h"

char *arith_buf, *arith_startbuf;

arith(s)
	char *s; 
{
	extern arith_wasoper;
	long result;

	arith_wasoper = 1;
	arith_buf = arith_startbuf = s;

	INTOFF;
	result = yyparse();
	arith_lex_reset();	/* reprime lex */
	INTON;

	return (result);
}

yyerror(s)
	char *s;
{
	extern yytext, yylval;

	yyerrok;
	yyclearin;
	arith_lex_reset();	/* reprime lex */
	error("arithmetic expression: %s: \"%s\"", s, arith_startbuf);
}

/*
 *  The exp(1) builtin.
 */
expcmd(argc, argv)
	char **argv;
{
	char *p;
	char *concat;
	char **ap;
	long i;

	if (argc > 1) {
		p = argv[1];
		if (argc > 2) {
			/*
			 * concatenate arguments
			 */
			STARTSTACKSTR(concat);
			ap = argv + 2;
			for (;;) {
				while (*p)
					STPUTC(*p++, concat);
				if ((p = *ap++) == NULL)
					break;
				STPUTC(' ', concat);
			}
			STPUTC('\0', concat);
			p = grabstackstr(concat);
		}
	} else
		p = "";

	i = arith(p);

	out1fmt("%d\n", i);
	return (! i);
}

/*************************/
#ifdef TEST_ARITH
#include <stdio.h>
main(argc, argv)
	char *argv[];
{
	printf("%d\n", exp(argv[1]));
}
error(s)
	char *s;
{
	fprintf(stderr, "exp: %s\n", s);
	exit(1);
}
#endif
