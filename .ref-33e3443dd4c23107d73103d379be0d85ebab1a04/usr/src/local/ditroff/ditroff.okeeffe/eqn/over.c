#ifndef lint
static char sccsid[] = "@(#)over.c	2.1 (CWI) 85/07/18";
#endif lint
# include "e.h"

boverb(p1, p2)
	int p1, p2;
{
	int treg;
	float h, b, d;

	treg = salloc();
	yyval = p1;
	d = EM(0.3, ps);
	h = eht[p1] + eht[p2] + d;
	b = eht[p2] - d;
	dprintf(".\tS%d <- %d over %d; b=%g, h=%g\n", 
		yyval, p1, p2, b, h);
	nrwid(p1, ps, p1);
	nrwid(p2, ps, p2);
	printf(".nr %d \\n(%d\n", treg, p1);
	printf(".if \\n(%d>\\n(%d .nr %d \\n(%d\n", p2, treg, treg, p2);
	printf(".nr %d \\n(%d+.5m\n", treg, treg);
	printf(".ds %d \\v'%gm'\\h'\\n(%du-\\n(%du/2u'\\*(%d\\\n", 
		yyval, REL(eht[p2]-ebase[p2]-d,ps), treg, p2, p2);
	printf("\\h'-\\n(%du-\\n(%du/2u'\\v'%gm'\\*(%d\\\n", 
		p2, p1, REL(-(eht[p2]-ebase[p2]+d+ebase[p1]),ps), p1);
	printf("\\h'-\\n(%du-\\n(%du/2u+.1m'\\v'%gm'\\l'\\n(%du-.2m'\\h'.1m'\\v'%gm'\n", 
		 treg, p1, REL(ebase[p1]+d,ps), treg, REL(d,ps));
	ebase[yyval] = b;
	eht[yyval] = h;
	lfont[yyval] = rfont[yyval] = 0;
	sfree(p2);
	sfree(treg);
}
