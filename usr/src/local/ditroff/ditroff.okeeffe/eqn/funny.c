#ifndef lint
static char *sccsid = "funny.c	(CWI)	1.1	85/03/01";
#endif
# include "e.h"
# include "e.def"

funny(n) int n; {
	char *f;

	yyval = oalloc();
	switch(n) {
	case SUM:
		f = "\\(*S"; break;
	case UNION:
		f = "\\(cu"; break;
	case INTER:	/* intersection */
		f = "\\(ca"; break;
	case PROD:
		f = "\\(*P"; break;
	default:
		error(FATAL, "funny type %d in funny", n);
	}
	printf(".ds %d \\s%d\\v'.3m'\\s+5%s\\s-5\\v'-.3m'\\s%d\n", yyval, ps, f, ps);
	eht[yyval] = VERT( EM(1.0, ps+5) - EM(0.2, ps) );
	ebase[yyval] = VERT( EM(0.3, ps) );
	if(dbg)printf(".\tfunny: S%d <- %s; h=%d b=%d\n", 
		yyval, f, eht[yyval], ebase[yyval]);
	lfont[yyval] = rfont[yyval] = ROM;
}
