# include "e.h"

sqrt(p2) int p2; {
	int nps;

	nps = EFFPS(((eht[p2]*9)/10+(res/POINT-1))/(res/POINT));
	yyval = p2;
	eht[yyval] = VERT( EM(1.2, nps) );
	if(dbg)printf(".\tsqrt: S%d <- S%d;b=%d, h=%d\n", 
		yyval, p2, ebase[yyval], eht[yyval]);
	if (rfont[yyval] == ITAL)
		printf(".as %d \\|\n", yyval);
	nrwid(p2, ps, p2);
	printf(".ds %d \\v'%du'\\s%d", yyval, ebase[p2], nps);	/* proper position for sqrt */
	printf("\\v'-.2m'\\(sr\\l'\\n(%du\\(rn'\\v'.2m'", p2);
	printf("\\s%d\\v'%du'\\h'-\\n(%du'\\*(%d\n", ps, -ebase[p2], p2, p2);
	lfont[yyval] = ROM;
}
