#ifndef lint
static char sccsid[] = "@(#)over.c	2.2 (CWI) 87/04/01";
#endif lint
# include "e.h"

boverb(p1, p2)
	int p1, p2;
{
	int treg;
	float h, b, d, d1, d2, d3;
	extern float Overgap, Overwid, Overline;

	treg = salloc();
	yyval = p1;
	d = EM(Overgap, ps);
	h = eht[p1] + eht[p2] + d;
	b = eht[p2] - d;
	dprintf(".\tS%d <- %d over %d; b=%g, h=%g\n", 
		yyval, p1, p2, b, h);
	nrwid(p1, ps, p1);
	nrwid(p2, ps, p2);
	printf(".nr %d \\n(%d\n", treg, p1);
	printf(".if \\n(%d>\\n(%d .nr %d \\n(%d\n", p2, treg, treg, p2);
	printf(".nr %d \\n(%d+%gm\n", treg, treg, Overwid);
	d2 = eht[p2]-ebase[p2]-d;	/* denom */
	printf(".ds %d \\v'%gm'\\h'\\n(%du-\\n(%du/2u'\\*(%d\\v'%gm'\\\n", 
		yyval, REL(d2,ps), treg, p2, p2, REL(-d2,ps));
	d1 = 2 * d + ebase[p1];		/* num */
	printf("\\h'-\\n(%du-\\n(%du/2u'\\v'%gm'\\*(%d\\v'%gm'\\\n", 
		p2, p1, REL(-d1,ps), p1, REL(d1,ps));
	printf("\\h'-\\n(%du-\\n(%du/2u+%gm'\\v'%gm'\\l'\\n(%du-%gm'\\h'%gm'\\v'%gm'\n", 
		treg, p1, Overline, REL(-d,ps),
		treg, 2*Overline, Overline, REL(d,ps));
	ebase[yyval] = b;
	eht[yyval] = h;
	lfont[yyval] = rfont[yyval] = 0;
	sfree(p2);
	sfree(treg);
}
