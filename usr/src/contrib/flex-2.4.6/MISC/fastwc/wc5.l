/* Oops; slight change from wc3.l introduces backtracking */

ws    [ \t]
nonws [^ \t\n]
word  {ws}*{nonws}+
words {word}{ws}+

%%
	int cc = 0, wc = 0, lc = 0;

{word}{ws}*		cc += yyleng; ++wc;
{word}{ws}*\n		cc += yyleng; ++wc; ++lc;
{words}{word}		cc += yyleng; wc += 2;	/* oops */
{words}{2}{word}{ws}*	cc += yyleng; wc += 3;
{words}{3}{word}{ws}*	cc += yyleng; wc += 4;

{ws}+			cc += yyleng;

\n+			cc += yyleng; lc += yyleng;

<<EOF>>		{
		printf( "%8d %8d %8d\n", lc, wc, cc );
		yyterminate();
		}
