/* Fastest version of wc: add rules to pick up newlines, too */

ws    [ \t]
nonws [^ \t\n]
word  {ws}*{nonws}+
words {word}{ws}+

%%
	int cc = 0, wc = 0, lc = 0;

{word}{ws}*		++wc; cc += yyleng;
{word}{ws}*\n		++wc; cc += yyleng; ++lc;
{words}{word}{ws}*	wc += 2; cc += yyleng;
{words}{word}{ws}*\n	wc += 2; cc += yyleng; ++lc;
{words}{2}{word}{ws}*	wc += 3; cc += yyleng;
{words}{2}{word}{ws}*\n	wc += 3; cc += yyleng; ++lc;
{words}{3}{word}{ws}*	wc += 4; cc += yyleng;
{words}{3}{word}{ws}*\n	wc += 4; cc += yyleng; ++lc;

{ws}+			cc += yyleng;

\n+			cc += yyleng; lc += yyleng;

<<EOF>>		{
		printf( "%8d %8d %8d\n", lc, wc, cc );
		yyterminate();
		}
