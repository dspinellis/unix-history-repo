#ifndef lint
static char sccsid[] = "@(#)move.c	2.2 (CWI) 87/04/01";
#endif lint
# include "e.h"
# include "y.tab.h"

move(dir, amt, p)
	int dir, amt, p;
{
	float a;

	yyval = p;
	a = EM(amt/100.0, ps);
	printf(".ds %d ", yyval);
	if (dir == FWD || dir == BACK)
		printf("\\h'%s%gm'\\*(%d\n", (dir==BACK) ? "-" : "", a, p);
	else if (dir == UP)
		printf("\\v'-%gm'\\*(%d\\v'%gm'\n", a, p, a);
	else if (dir == DOWN)
		printf("\\v'%gm'\\*(%d\\v'-%gm'\n", a, p, a);
	dprintf(".\tmove %d dir %d amt %g; h=%g b=%g\n", 
		p, dir, a, eht[yyval], ebase[yyval]);
}
