#ifndef lint
static char sccsid[] = "@(#)sqrt.c	2.1 (CWI) 85/07/18";
#endif lint
# include "e.h"

sqrt(p2)
	int p2;
{
	int nps;

	nps = ps * 0.95 * eht[p2] / EM(1.0,ps) + 0.99;	/* kludgy */
	nps = max(EFFPS(nps), ps);
	yyval = p2;
	if (ttype != DEV202)
		eht[yyval] = EM(1.2, nps);
	else
		eht[yyval] = EM(1.1, nps);
	dprintf(".\tS%d <- sqrt S%d;b=%g, h=%g, nps=%d\n", 
		yyval, p2, ebase[yyval], eht[yyval], nps);
	printf(".as %d \\|\n", yyval);
	nrwid(p2, ps, p2);
	printf(".ds %d \\v'%gm'%s", yyval, REL(ebase[p2],ps), DPS(ps,nps));	/* proper position for sqrt */
	if (ttype != DEV202)
		printf("\\v'-.2m'\\(sr\\l'\\n(%du\\(rn'\\v'.2m'", p2);
	else
		printf("\\(sr\\l'\\n(%du\\(rn'", p2);
	printf("%s\\v'%gm'\\h'-\\n(%du'\\*(%d\n", DPS(nps,ps), REL(-ebase[p2],ps), p2, p2);
	lfont[yyval] = rfont[yyval] = ROM;
}
