# include "e.h"

sqrt(p2) int p2; {
	int nps;
	nps = EFFPS(((eht[p2]*9)/10+5)/6);
	yyval = p2;
	eht[yyval] = VERT( (nps*6*12)/10 );
	if(dbg)printf(".\tsqrt: S%d <- S%d;b=%d, h=%d\n", 
		yyval, p2, ebase[yyval], eht[yyval]);
	if (rfont[yyval] == ITAL)
		printf(".as %d \\|\n", yyval);
	nrwid(p2, ps, p2);
	printf(".ds %d \\v'%du'\\s%d\\v'-.2m'\\(sr\\l'\\n(%du\\(rn'\\v'.2m'\\s%d", 
		yyval, ebase[p2], nps, p2, ps);
	printf("\\v'%du'\\h'-\\n(%du'\\*(%d\n", -ebase[p2], p2, p2);
	lfont[yyval] = ROM;
}
