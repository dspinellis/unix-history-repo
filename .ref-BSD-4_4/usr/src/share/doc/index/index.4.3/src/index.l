%{
#include "reduce.h"
#include "y.tab.h"
int lineno = 1;  /* since recognition involves readahead, this is
 usually a few greater than where the error occurs, like at the beginning
 of the next group */
extern char text[];
extern int debug;
%}
B [ ]
W [ \t]
D [^ \t\n]
DNP [^ .\t\n]
AN [-A-Za-z0-9_+]
A [A-Za-z_]
%%
^{D}+ 	{
		/* index head entry */
#define DEBUG
#ifdef DEBUG
		if(debug>14) {
		printf ("HEAD");
		ECHO;
		}
#endif
		yylval.sptr = &yytext[0];
		return HEADWORD;
}
{D}* {
		/* words in an entry */
#ifdef DEBUG
		if(debug>14) {
		printf ("WORD");
		ECHO;
		}
#endif
		yylval.sptr = &yytext[0];
		return WORD;
}
"..." {
		/* ellipsis in long index entries */
#ifdef DEBUG
		if(debug>14) {
		printf ("...");
		ECHO;
		}
#endif
		yylval.sptr = &yytext[0];
		return ELLIPSIS;
}
{W} {
#ifdef DEBUG
		ECHO;
#endif
		strcat(text," ");  /* kludge, should be returned */
		;
}
^{W}+\"{W}+	{
		/* index entry after head */
#ifdef DEBUG
		if(debug>14) {
		printf ("DUP");
		ECHO;
		}
#endif
		return DUP;
}
{W}{W}+	{
		/* white space */
#ifdef DEBUG
		if(debug>14) {
		printf("WHITE");
		ECHO;
		}
#endif
		return WHITESPACE;
}
{AN}+"."{AN}+:{W}+[0-9]+%.*$	{
		/* page reference */
#ifdef DEBUG
		if(debug>14) {
		printf("PAGE");
		ECHO;
		}
#endif
		yylval.pptr = &yytext[0];
		return PAGE;
}
\n	{
	lineno++; 
#ifdef DEBUG
		if(debug>14) {
		printf("NL");
		ECHO;
		}
#endif
	return  '\n';
}
.	{
#ifdef DEBUG
		if(debug>14) {
		printf("OTHER");
		ECHO;
		}
#endif
	return yytext[0];
}

%%
/* user routines */
