/* First cut at a flex-based "wc" tool. */

ws    [ \t]
nonws [^ \t\n]

%%
	int cc = 0, wc = 0, lc = 0;

{nonws}+	cc += yyleng; ++wc;

{ws}+		cc += yyleng;

\n		++lc; ++cc;

<<EOF>>		{
		printf( "%8d %8d %8d\n", lc, wc, cc );
		yyterminate();
		}
