#ifndef lint
static char sccsid[] = "@(#)diacrit.c	2.2 (CWI) 87/04/01";
#endif lint
#include "e.h"
#include "y.tab.h"

extern float Dvshift, Dhshift, Dh2shift, Dheight, Barv, Barh;
extern char *Vec, *Dyad, *Hat, *Tilde, *Dot, *Dotdot, *Utilde;

diacrit(p1, type)
	int p1, type;
{
	int c, t;

	c = salloc();
	t = salloc();
	nrwid(p1, ps, p1);
	printf(".nr 10 %gm\n", max(REL(eht[p1]-ebase[p1]-1,ps), 0));	/* vert shift if high */
	if (type == HIGHBAR)
		printf(".nr 10 \\n(10+%gm\n", Dvshift);
	else if (type == LOWBAR)
		printf(".nr 10 0\n");
	else
		printf(".if \\n(ct>1 .nr 10 \\n(10+%gm\n", Dvshift);
	printf(".nr %d %gm\n", t, Dhshift);	/* horiz shift if high */
	printf(".if \\n(ct>1 .nr %d %gm\n", t, Dh2shift);	/* was .1 and .15 */
	switch (type) {
	case VEC:
		printf(".ds %d %s\n", c, Vec);
		break;
	case DYAD:
		printf(".ds %d %s\n", c, Dyad);
		break;
	case HAT:
		printf(".ds %d %s\n", c, Hat);
		break;
	case TILDE:
		printf(".ds %d %s\n", c, Tilde);
		break;
	case DOT:
		printf(".ds %d %s\n", c, Dot);
		break;
	case DOTDOT:
		printf(".ds %d %s\n", c, Dotdot);
		break;
	case BAR:
	case LOWBAR:
	case HIGHBAR:
		printf(".ds %d \\v'%gm'\\h'%gm'\\l'\\n(%du-%gm'\\h'%gm'\\v'%gm'\n",
			c, -Barv, Barh, p1, 2*Barh, Barh, Barv);
		break;
	case UNDER:
		printf(".ds %d \\l'\\n(%du\\(ul'\n", c, p1);
		printf(".nr %d 0\n", t);
		printf(".nr 10 0-.1m-%gm\n", REL(ebase[p1],ps));
		printf(".if \\n(ct%%2=1 .nr 10 0\\n(10-.25m\n");
		break;
	case UTILDE:
		printf(".ds %d %s\n", c, Utilde);
		printf(".nr %d 0\n", t);
		printf(".nr 10 0-%gm\n", REL(ebase[p1],ps));
		printf(".if \\n(ct%%2=1 .nr 10 0\\n(10-%gm\n", Dvshift);
		break;
	}
	nrwid(c, ps, c);
	if (lfont[p1] != ITAL)
		printf(".nr %d 0\n", t);
	printf(".as %d \\h'-\\n(%du-\\n(%du/2u+\\n(%du'\\v'0-\\n(10u'\\*(%d", 
		p1, p1, c, t, c);
	printf("\\v'\\n(10u'\\h'-\\n(%du+\\n(%du/2u-\\n(%du'\n", c, p1, t);
	if (type != UNDER && type != UTILDE)
		eht[p1] += EM(Dheight, ps);	/* was .15 */
	dprintf(".\tdiacrit: %c over S%d, lf=%c, rf=%c, h=%g, b=%g\n",
		type, p1, lfont[p1], rfont[p1], eht[p1], ebase[p1]);
	sfree(c); sfree(t);
}
