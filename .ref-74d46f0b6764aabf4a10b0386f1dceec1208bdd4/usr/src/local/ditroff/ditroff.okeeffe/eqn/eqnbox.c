#ifndef lint
static char sccsid[] = "@(#)eqnbox.c	2.2 (CWI) 87/04/01";
#endif lint
# include "e.h"

eqnbox(p1, p2, lu)
{
	float b, h;
	char *sh;
	extern char *IRspace;

	yyval = p1;
	b = max(ebase[p1], ebase[p2]);
	eht[yyval] = h = b + max(eht[p1]-ebase[p1], 
		eht[p2]-ebase[p2]);
	ebase[yyval] = b;
	dprintf(".\tS%d <- %d %d; b=%g, h=%g\n", yyval, p1, p2, b, h);
	if (rfont[p1] == ITAL && lfont[p2] == ROM)
		sh = IRspace;	/* was \| */
	else
		sh = "";
	if (lu) {
		printf(".nr %d \\w'\\*(%d%s'\n", p1, p1, sh);
		printf(".ds %d \\h'|\\n(09u-\\n(%du'\\*(%d\n", p1, p1, p1);
	}
	printf(".as %d \"%s\\*(%d\n", yyval, sh, p2);
	rfont[p1] = rfont[p2];
	sfree(p2);
}
