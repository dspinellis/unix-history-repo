#ifndef lint
static char sccsid[] = "@(#)shift.c	2.1 (CWI) 85/07/18";
#endif lint
#include "e.h"
#include "e.def"

subsup(p1, p2, p3)
	int p1, p2, p3;
{
	if (p2 != 0 && p3 != 0)
		shift2(p1, p2, p3);
	else if (p2 != 0)
		bshiftb(p1, SUB, p2);
	else if (p3 != 0)
		bshiftb(p1, SUP, p3);
}

bshiftb(p1, dir, p2)
	int p1, dir, p2;
{
	int subps;
	float shval, d1, h1, b1, h2, b2;
	char *sh1, *sh2;

	yyval = p1;
	h1 = eht[p1];
	b1 = ebase[p1];
	h2 = eht[p2];
	b2 = ebase[p2];
	subps = ps;
	ps += deltaps;
	sh1 = sh2 = "";
	if (dir == SUB) {
		/* base .2m below bottom of main box */
		shval = b1 + EM(0.2, ps);
		ebase[yyval] = shval + b2;
		eht[yyval] = max(h1-b1+shval+b2, h2);
		if (rfont[p1] == ITAL && lfont[p2] == ROM)
			sh1 = "\\|";
	} else {	/* superscript */
		/* 4/10 up main box */
		d1 = EM(0.2, subps);
		ebase[yyval] = b1;
		shval = -(0.4 * (h1-b1)) - b2;
		if (0.4*(h1-b1) + h2 < h1-b1)	/* raise little super */
			shval = -(h1-b1) + h2-b2 - d1;
		eht[yyval] = h1 + max(0, h2 - 0.6*(h1-b1));
		if (rfont[p1] == ITAL)
			sh1 = "\\|";
	}
	dprintf(".\tS%d <- %d shift %g %d; b=%g, h=%g, ps=%d, subps=%d\n", 
		yyval, p1, shval, p2, ebase[yyval], eht[yyval], ps, subps);
	sh2 = "\\^";
	printf(".as %d \\v'%gm'%s%s\\*(%d%s%s\\v'%gm'\n", 
		yyval, REL(shval,ps), DPS(ps,subps), sh1, p2,
		DPS(subps,ps), sh2, REL(-shval,ps));
	rfont[p1] = 0;
	sfree(p2);
}

shift2(p1, p2, p3)
	int p1, p2, p3;
{
	int subps;
	float h1, h2, h3, b1, b2, b3, subsh, d1, d2, supsh;
	int treg;

	treg = salloc();
	yyval = p1;
	subps = ps;	/* sub and sup at this size */
	ps += deltaps;	/* outer size */
	h1 = eht[p1]; b1 = ebase[p1];
	h2 = eht[p2]; b2 = ebase[p2];
	h3 = eht[p3]; b3 = ebase[p3];
	subsh = EM(0.2, ps);
	if (b1 > b2 + subsh) /* move little sub down */
		subsh += b1;
	eht[yyval] = max(subsh+b2-b1+h1, h2);
	supsh = -0.4*(h1-b1) - b3;
	d2 = EM(0.2, subps);
	if (h3 < 0.6*(h1-b1))
		supsh = -(h1-b1) + (h3-b3) - d2;
	ebase[yyval] = subsh + b2 - b1;
	eht[yyval] = h1 + subsh+b2-b1 + max(0, h3-0.6*(h1-b1));
	dprintf(".\tS%d <- %d sub %d sup %d, ps=%d, subps=%d, h=%g, b=%g\n",
		yyval, p1, p2, p3, ps, subps, eht[yyval], ebase[yyval]);
	/* if (rfont[p1] == ITAL && lfont[p2] == ROM) */
	/* if (lfont[p2] == ROM) */
		printf(".ds %d \\^\\*(%d\n", p2, p2);
	/* if (rfont[p2] == ITAL)
		printf(".as %d \\^\n", p2); */
	nrwid(p2, subps, p2);
	printf(".ds %d \\^\\*(%d\n", p3, p3);
	nrwid(p3, subps, p3);
	printf(".nr %d \\n(%d\n", treg, p3);
	printf(".if \\n(%d>\\n(%d .nr %d \\n(%d\n", p2, treg, treg, p2);
	printf(".as %d \\v'%gm'%s\\*(%d\\h'-\\n(%du'\\v'%gm'\\\n", 
		p1, REL(subsh,ps), DPS(ps,subps), p2, p2, REL(-subsh+supsh,subps));
	printf("\\*(%d\\h'-\\n(%du+\\n(%du'%s\\v'%gm'\\^\n", 
		p3, p3, treg, DPS(subps,ps), REL(-supsh,ps));
	if (rfont[p2] == ITAL)
		rfont[yyval] = 0;	/* lie */
	sfree(p2); sfree(p3); sfree(treg);
}
