%{
#ifndef lint
static char RCSid[] = "$Header: scanner.l,v 2.1 86/07/29 06:48:21 jqj Exp $";
#endif
/*
 * scanner.l -- scanner for the XNS courier compiler
 */

/* $Log:	scanner.l,v $
 * Revision 2.1  86/07/29  06:48:21  jqj
 * added support for 37D style comments.
 * 
 * Revision 2.0  85/11/21  07:21:45  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/20  12:56:17  jqj
 * Initial revision
 * 
 * Revision 1.3  85/03/11  16:40:08  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:05:51  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:58  jqj
 * Initial revision
 * 
 */

#include "compiler.h"
#include "y.tab.h"
%}
%%
"--"$			|
"----"			|
\-(\-[^\n-]+)+(\n|"--")	|
[ \t\n]			/* whitespace */;
ARRAY			{ return (ARRAY); }
BEGIN			{ return (_BEGIN); }
BOOLEAN			{ return (BOOLEAN); }
CARDINAL		{ return (CARDINAL); }
CHOICE			{ return (CHOICE); }
DEPENDS			{ return (DEPENDS); }
END			{ return (END); }
ERROR			{ return (ERROR); }
INTEGER			{ return (INTEGER); }
LONG			{ return (LONG); }
OF			{ return (OF); }
PROCEDURE		{ return (PROCEDURE); }
PROGRAM			{ return (PROGRAM); }
RECORD			{ return (RECORD); }
REPORTS			{ return (REPORTS); }
RETURNS			{ return (RETURNS); }
SEQUENCE		{ return (SEQUENCE); }
STRING			{ return (STRING); }
TYPE			{ return (TYPE); }
UNSPECIFIED		{ return (UNSPECIFIED); }
UPON			{ return (UPON); }
VERSION			{ return (VERSION); }
TRUE			{ return (TRUE); }
FALSE			{ return (FALSE); }
"=>"			{ return (_CHOOSES); }
"-"?[0-9]+		{
				/*
				 * decimal constant.
				 */
				yylval.stringvalue = copy(yytext);
				return (number);
			}
"-"?[0-9]+[Dd]		{
				/*
				 * decimal constant.
				 */
				yytext[yyleng-1] = '\0';
				yylval.stringvalue = copy(yytext);
				return (number);
			}
"-"?[0-7]+[Bb]		{
				char buf[BUFSIZ];
				/*
				 * octal constant.
				 * change to C representation
				 */
				yytext[yyleng-1] = '\0';
				if (*yytext != '-')
					sprintf(buf,"0%s", yytext);
				else
					sprintf(buf,"-0%s", yytext+1);
				yylval.stringvalue = copy(buf);
				return (number);
			}
"-"?[0-9][0-9A-Fa-f]*[Xx]	{
				char buf[BUFSIZ];
				/*
				 * hex constant.
				 * change to C representation
				 */
				yytext[yyleng-1] = '\0';
				if (*yytext != '-')
					sprintf(buf,"0x%s", yytext);
				else
					sprintf(buf,"-0x%s", yytext+1);
				yylval.stringvalue = copy(buf);
				return (number);
			}
\"[^"\n"]*\"		{
				/*
				 * string constant
				 */
				
				yylval.stringvalue = copy(yytext);
				return (string);
			}
\"([^\n"]|\"\")*\"	{
				register char *p;
				/*
				 * string constant with embedded ""
				 */

				for (p=yytext+1; p<yytext+yyleng-1; p++)
					if (*p == '"') *p++='\\';
				yylval.stringvalue = copy(yytext);
				return (string);
			}
[a-zA-Z_][a-zA-Z0-9_]*	{
				yylval.stringvalue = copy(yytext);
				return (identifier);
			}
.			{
				return ((int) yytext[0]);
			}

%%
