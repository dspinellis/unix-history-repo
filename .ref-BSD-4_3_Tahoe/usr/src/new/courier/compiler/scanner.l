%{
#ifndef lint
static char sccsid[] = "@(#)scanner.l	4.2 (Berkeley) 7/7/83";
#endif

#include "Courier.h"
#include "y.tab.h"
%}

%%

--.*\n		;
[ \t\n]		;
ARRAY		{ return (ARRAY); }
BEGIN		{ return (_BEGIN); }
BOOLEAN		{ return (BOOLEAN); }
CARDINAL	{ return (CARDINAL); }
CHOICE		{ return (CHOICE); }
DEPENDS		{ return (DEPENDS); }
END		{ return (END); }
ERROR		{ return (ERROR); }
INTEGER		{ return (INTEGER); }
LONG		{ return (LONG); }
OF		{ return (OF); }
PROCEDURE	{ return (PROCEDURE); }
PROGRAM		{ return (PROGRAM); }
RECORD		{ return (RECORD); }
REPORTS		{ return (REPORTS); }
RETURNS		{ return (RETURNS); }
SEQUENCE	{ return (SEQUENCE); }
STRING		{ return (STRING); }
TYPE		{ return (TYPE); }
UNSPECIFIED	{ return (UNSPECIFIED); }
UPON		{ return (UPON); }
VERSION		{ return (VERSION); }

-?[0-9]+			{
				/*
				 * Only decimal numbers are supported.
				 */
				yylval.object = make(O_CONSTANT, atoi(yytext));
				return (number);
			}

[a-zA-Z_][a-zA-Z0-9_]*	{
				yylval.object = make(O_SYMBOL, yytext);
				return (identifier);
			}

.			{
				return ((int) yytext[0]);
			}

%%
