/* Somewhat faster "wc" tool: match more text with each rule */

ws    [ \t]
nonws [^ \t\n]
word  {ws}*{nonws}+

%%
	int cc = 0, wc = 0, lc = 0;

{word}{ws}*	cc += yyleng; ++wc;
{word}{ws}*\n	cc += yyleng; ++wc; ++lc;

{ws}+		cc += yyleng;

\n+		cc += yyleng; lc += yyleng;

<<EOF>>		{
		printf( "%8d %8d %8d\n", lc, wc, cc );
		yyterminate();
		}
