#ifndef lint
static char sccsid[] = "@(#)diacrit.c	2.1 (CWI) 85/07/18";
#endif lint
#include "e.h"
#include "e.def"

diacrit(p1, type)
	int p1, type;
{
	int c, t;

	c = salloc();
	t = salloc();
	nrwid(p1, ps, p1);
	printf(".nr 10 %gm\n", max(REL(eht[p1]-ebase[p1]-1,ps), 0));	/* vert shift if high */
	printf(".if \\n(ct>1 .nr 10 \\n(10+.25m\n");
	printf(".nr %d .025m\n", t);	/* horiz shift if high */
	printf(".if \\n(ct>1 .nr %d .05m\n", t);	/* was .1 and .15 */
	switch (type) {
	case VEC:
		printf(".ds %d \\v'-.45m'\\s-1\\(->\\s0\\v'.45m'\n", c);
		break;
	case DYAD:
		printf(".ds %d \\v'-.45m'\\s-1\\z\\(<-\\|\\(->\\s0\\v'.45m'\n", c);
		break;
	case HAT:
		printf(".ds %d \\v'-.1m'\\s+1^\\s0\\v'.1m'\n", c);
		break;
	case TILDE:
		printf(".ds %d \\v'-.1m'\\s+1~\\s0\\v'.1m'\n", c);
		break;
	case DOT:
		printf(".ds %d \\v'-.67m'.\\v'.67m'\n", c);
		break;
	case DOTDOT:
		printf(".ds %d \\v'-.67m'..\\v'.67m'\n", c);
		break;
	case BAR:
		printf(".ds %d \\v'-.68m'\\h'.05m'\\l'\\n(%du-.1m'\\h'.05m'\\v'.68m'\n",
			c, p1);
		break;
	case UNDER:
		printf(".ds %d \\l'\\n(%du\\(ul'\n", c, p1);
		printf(".nr %d 0\n", t);
		printf(".nr 10 0-.1m-%gm\n", REL(ebase[p1],ps));
		printf(".if \\n(ct%%2=1 .nr 10 0\\n(10-.25m\n");
		break;
	case UTILDE:
		printf(".ds %d \\v'1.0m'\\s+2~\\s-2\\v'-1.0m'\n", c);
		printf(".nr %d 0\n", t);
		printf(".nr 10 0-%gm\n", REL(ebase[p1],ps));
		printf(".if \\n(ct%%2=1 .nr 10 0\\n(10-.25m\n");
		break;
	}
	nrwid(c, ps, c);
	if (lfont[p1] != ITAL)
		printf(".nr %d 0\n", t);
	printf(".as %d \\h'-\\n(%du-\\n(%du/2u+\\n(%du'\\v'0-\\n(10u'\\*(%d", 
		p1, p1, c, t, c);
	printf("\\v'\\n(10u'\\h'-\\n(%du+\\n(%du/2u-\\n(%du'\n", c, p1, t);
	if (type != UNDER && type != UTILDE)
		eht[p1] += EM(0.25, ps);	/* was .15 */
	dprintf(".\tdiacrit: %c over S%d, lf=%c, rf=%c, h=%g, b=%g\n",
		type, p1, lfont[p1], rfont[p1], eht[p1], ebase[p1]);
	sfree(c); sfree(t);
}
