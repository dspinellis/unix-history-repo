#ifndef lint
static char sccsid[] = "@(#)paren.c	2.2 (CWI) 87/04/01";
#endif lint
# include "e.h"

paren(leftc, p1, rightc)
	int p1, leftc, rightc;
{
	int n, m, j;
	float h1, b1, v;
	extern float Parenbase;

	h1 = eht[p1];
	b1 = ebase[p1];
	yyval = p1;
	lfont[yyval] = rfont[yyval] = 0;
	n = REL(h1,ps) + 0.99;	/* ceiling */
	if (n < 2)
		n = 1;
	m = n - 2;
	if (leftc == '{' || rightc == '}') {
		n = n%2 ? n : ++n;
		if (n < 3)
			n = 3;
		m = n-3;
	}
	eht[yyval] = EM((float) n, ps);
	ebase[yyval] = eht[yyval]/2 - EM(Parenbase, ps);
	/* v = REL(-ebase[yyval] + (eht[yyval]-h1)/2 + b1, ps); */
	v = 0;	/* in other words, don't shift it at all */
	printf(".ds %d \\|", yyval);
	switch (leftc) {
	case 'n':	/* nothing */
	case '\0':
		break;
	case 'f':	/* floor */
		if (n <= 1)
			printf("\\(lf");
		else
			brack(m, "\\(bv", "\\(bv", "\\(lf");
		break;
	case 'c':	/* ceiling */
		if (n <= 1)
			printf("\\(lc");
		else
			brack(m, "\\(lc", "\\(bv", "\\(bv");
		break;
	case '{':
		printf("\\b'\\(lt");
		for(j = 0; j < m; j += 2) printf("\\(bv");
		printf("\\(lk");
		for(j = 0; j < m; j += 2) printf("\\(bv");
		printf("\\(lb'");
		break;
	case '(':
		brack(m, "\\(lt", "\\(bv", "\\(lb");
		break;
	case '[':
		brack(m, "\\(lc", "\\(bv", "\\(lf");
		break;
	case '|':
		brack(m, "|", "|", "|");
		break;
	default:
		brack(m, (char *) &leftc, (char *) &leftc, (char *) &leftc);
		break;
	}
	if (v)
		printf("\\v'%gm'\\*(%d\\v'%gm'", -v, p1, v);
	else
		printf("\\*(%d", p1);
	if (rightc) {
		switch (rightc) {
		case 'f':	/* floor */
			if (n <= 1)
				printf("\\(rf");
			else
				brack(m, "\\(bv", "\\(bv", "\\(rf");
			break;
		case 'c':	/* ceiling */
			if (n <= 1)
				printf("\\(rc");
			else
				brack(m, "\\(rc", "\\(bv", "\\(bv");
			break;
		case '}':
			printf("\\b'\\(rt");
			for(j = 0; j < m; j += 2) printf("\\(bv");
			printf("\\(rk");
			for(j = 0; j < m; j += 2) printf("\\(bv");
			printf("\\(rb'");
			break;
		case ']':
			brack(m, "\\(rc", "\\(bv", "\\(rf");
			break;
		case ')':
			brack(m, "\\(rt", "\\(bv", "\\(rb");
			break;
		case '|':
			brack(m, "|", "|", "|");
			break;
		default:
			brack(m, (char *) &rightc, (char *) &rightc, (char *) &rightc);
			break;
		}
	}
	printf("\n");
	dprintf(".\tcurly: h=%g b=%g n=%d v=%g l=%c, r=%c\n", 
		eht[yyval], ebase[yyval], n, v, leftc, rightc);
}

brack(m, t, c, b)
	int m;
	char *t, *c, *b;
{
	int j;
	printf("\\b'%s", t);
	for( j=0; j < m; j++)
		printf("%s", c);
	printf("%s'", b);
}
