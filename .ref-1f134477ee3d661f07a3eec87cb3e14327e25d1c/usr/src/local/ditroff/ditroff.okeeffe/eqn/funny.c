#ifndef lint
static char sccsid[] = "@(#)funny.c	2.2 (CWI) 87/04/01";
#endif lint
# include "e.h"
# include "y.tab.h"

extern int Funnyps;
extern float Funnyht, Funnybase;
extern char *Sum, *Union, *Inter, *Prod;

funny(n)
	int n;
{
	char *f;

	yyval = salloc();
	switch(n) {
	case SUM:
		f = Sum; break;
	case UNION:
		f = Union; break;
	case INTER:	/* intersection */
		f = Inter; break;
	case PROD:
		f = Prod; break;
	default:
		error(FATAL, "funny type %d in funny", n);
	}
	printf(".ds %d %s\n", yyval, f);
	eht[yyval] = EM(1.0, ps+Funnyps) - EM(Funnyht, ps);
	ebase[yyval] = EM(Funnybase, ps);
	eps[yyval] = ps;
	dprintf(".\tS%d <- %s; h=%g b=%g\n", 
		yyval, f, eht[yyval], ebase[yyval]);
	lfont[yyval] = rfont[yyval] = ROM;
}
