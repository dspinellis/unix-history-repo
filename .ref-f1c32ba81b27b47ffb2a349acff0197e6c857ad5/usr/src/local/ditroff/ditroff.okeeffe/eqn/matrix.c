#ifndef lint
static char sccsid[] = "@(#)matrix.c	2.2 (CWI) 87/04/01";
#endif lint
#include "e.h"

startcol(type)	/* mark start of column in lp[] array */
	int type;
{
	int oct = ct;

	lp[ct++] = type;
	lp[ct++] = 0;	/* count, to come */
	lp[ct++] = 0;	/* separation, to come */
	return oct;
}

column(oct, sep)	/* remember end of column that started at lp[oct] */
	int oct, sep;
{
	int i, type;

	lp[oct+1] = ct - oct - 3;
	lp[oct+2] = sep;
	type = lp[oct];
	if (dbg) {
		printf(".\t%d column of", type);
		for (i = oct+3; i < ct; i++ )
			printf(" S%d", lp[i]);
		printf(", rows=%d, sep=%d\n", lp[oct+1], lp[oct+2]);
	}
}

matrix(oct)	/* matrix is list of columns */
	int oct;
{
	int nrow, ncol, i, j, k, val[100];
	float b, hb;
	char *space;
	extern char *Matspace;

	space = Matspace;	/* between columns of matrix */
	nrow = lp[oct+1];	/* disaster if rows inconsistent */
				/* also assumes just columns */
				/* fix when add other things */
	ncol = 0;
	for (i = oct+1; i < ct; i += lp[i]+3 ) {
		ncol++;
		dprintf(".\tcolct=%d\n", lp[i]);
	}
	for (k=1; k <= nrow; k++) {
		hb = b = 0;
		j = oct + k + 2;
		for (i=0; i < ncol; i++) {
			hb = max(hb, eht[lp[j]]-ebase[lp[j]]);
			b = max(b, ebase[lp[j]]);
			j += nrow + 3;
		}
		dprintf(".\trow %d: b=%g, hb=%g\n", k, b, hb);
		j = oct + k + 2;
		for (i=0; i<ncol; i++) {
			ebase[lp[j]] = b;
			eht[lp[j]] = b + hb;
			j += nrow + 3;
		}
	}
	j = oct;
	for (i=0; i<ncol; i++) {
		pile(j);
		val[i] = yyval;
		j += nrow + 3;
	}
	yyval = salloc();
	eht[yyval] = eht[val[0]];
	ebase[yyval] = ebase[val[0]];
	lfont[yyval] = rfont[yyval] = 0;
	dprintf(".\tmatrix S%d: r=%d, c=%d, h=%g, b=%g\n",
		yyval,nrow,ncol,eht[yyval],ebase[yyval]);
	printf(".ds %d \"", yyval);
	for( i=0; i<ncol; i++ )  {
		printf("\\*(%d%s", val[i], i==ncol-1 ? "" : space);
		sfree(val[i]);
	}
	printf("\n");
}
