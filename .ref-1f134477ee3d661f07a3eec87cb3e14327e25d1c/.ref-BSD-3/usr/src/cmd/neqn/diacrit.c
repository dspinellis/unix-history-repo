# include "e.h"
# include "e.def"

diacrit(p1, type) int p1, type; {
	int c, t;

	c = oalloc();
	t = oalloc();
	nrwid(p1, ps, p1);
	printf(".nr 10 %du\n", max(eht[p1]-ebase[p1]-VERT(2),0));
	switch(type) {
		case VEC:	/* vec */
		case DYAD:	/* dyad */
			printf(".ds %d \\v'-1'_\\v'1'\n", c);
			break;
		case HAT:
			printf(".ds %d ^\n", c);
			break;
		case TILDE:
			printf(".ds %d ~\n", c);
			break;
		case DOT:
			printf(".ds %d \\v'-1'.\\v'1'\n", c);
			break;
		case DOTDOT:
			printf(".ds %d \\v'-1'..\\v'1'\n", c);
			break;
		case BAR:
			printf(".ds %d \\v'-1'\\l'\\n(%du'\\v'1'\n", 
				c, p1);
			break;
		case UNDER:
			printf(".ds %d \\l'\\n(%du'\n", c, p1);
			break;
		}
	nrwid(c, ps, c);
	printf(".as %d \\h'-\\n(%du-\\n(%du/2u'\\v'0-\\n(10u'\\*(%d", 
		p1, p1, c, c);
	printf("\\v'\\n(10u'\\h'-\\n(%du+\\n(%du/2u'\n", c, p1);
	if (type != UNDER)
		eht[p1] += VERT(1);
	if (dbg) printf(".\tdiacrit: %c over S%d, h=%d, b=%d\n", type, p1, eht[p1], ebase[p1]);
	ofree(c); ofree(t);
}
