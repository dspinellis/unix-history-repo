#include "ne.h"

int	markpos;

mark(n) int n; {
	if( n )
		markpos = ewid[n];
	else {
		yyval = oalloc();
		printf(".ds %d \"\n", yyval);
		ebase[yyval] = ewid[yyval] = markpos = 0;
		eht[yyval] = 2;
	}
	if(dbg)printf(".\tmark %d as %d\n", n, markpos);
}

lineup(n) int n; {
	if( n ) {
		printf(".ds %d \"", n);
		fwd(markpos-ewid[n]);
		printf("\\*(%d\n", n);
		ewid[n] = markpos;
	}
	else {
		yyval = oalloc();
		printf(".ds %d \"", yyval);
		fwd(markpos);
		printf("\n");
		ebase[yyval] = 0;
		eht[yyval] = 2;
		ewid[yyval] = markpos;
	}
	if(dbg)printf(".\tlineup %d at %d\n", n, markpos);
}
