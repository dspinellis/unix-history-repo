/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.1 February 1978
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.1 February 1978
 */

#include "whoami"
#include "0.h"
#include "yy.h"

/*
 * Parser for 'yacc' output.
 * Specifially Modified for Berkeley Pascal
 */

int	yystate;	/* Current parser state */
int	*yypv;
unsigned yytshifts 1;	/* Number of "true" shifts */

/*
 * Parse Tables
 */
int	yygo[];
int	yypgo[];
int	yyr1[];
int	yyr2[];
int	yyact[];
int	yypact[];

/*
 * Parse and parallel semantic stack
 */
int	yyv[MAXDEPTH];
int	yys[MAXDEPTH];

/*
 * This routine parses the input stream, and
 * returns if it accepts, or if an unrecoverable syntax
 * error is encountered.
 */
yyparse()
{
	register int *ps, n, *p;
	int paniced, *panicps, idfail;

	yystate = 0;
	yychar = yylex();
	OY.Yychar = -1;
	yyshifts = 3;
	paniced = 0;
	ps = &yys[0]-1;
	yypv = &yyv[0]-1;
#ifdef PXP
	yypw = &yyw[0]-1;
#endif

stack:
	/*
	 * Push new state and value.
	 */
	if (yypv >= &yyv[MAXDEPTH-1]) {
		yerror("Parse stack overflow");
		pexit(DIED);
	}
	*++ps = yystate;
	*++yypv = yyval;
#ifdef PXP
	yypw++;
#endif
newstate:
	/*
	 * Locate parsing actions for the
	 * new parser state.
	 */
	p = &yyact[ yypact[yystate+1] ]; 
actn:
	/*
	 * Search the parse actions table
	 * for something useful to do.
	 * While n is non-positive, it is the negation
	 * of the token we are testing for.
	 */
#ifdef PI
	if ((n = *p++) <= 0) {
		if (yychar < 0)
			yychar = yylex();
		do
			if ((n =+ yychar) != 0)
				p++;
		while ((n = *p++) <= 0);
	}
#else
	while ((n = *p++) <= 0)
		if ((n =+ yychar) != 0)
			p++;
#endif
	switch (n >> 12) {

		/*
		 * Shift.
		 */
		case 2:
#ifdef PXP
			yypw[1].Wseqid = yyseqid;
			yypw[1].Wcol = yycol;
#endif
			OYcopy();
			yystate = n & 07777;
			yyval = yylval;
#ifdef PI
			yychar = -1;
#else
			yychar = yylex();
#endif
			yyshifts++;
			yytshifts++;
			goto stack;

		/*
		 * Reduce.
		 */
		case 3:
			n =& 07777;
			N = yyr2[n];
			if (N == 1 && OY.Yychar == YID && !yyEactr(n, yypv[0])) {
				idfail = 1;
				goto errin;
			}
			OY.Yychar = -1;
			ps =- N;
			yypv =- N;
#ifdef PXP
			yypw =- N;
#endif
			yyval = yypv[1];
			yyactr(n);
			/*
			 * Use goto table to find next state.
			 */
			p = &yygo[yypgo[yyr1[n]]];
			while (*p != *ps && *p >= 0)
				p =+ 2;
			yystate = p[1];
			goto stack;

		/*
		 * Accept.
		 */
		case 4:
			return;

		/*
		 * Error.
		 */
		case 1:
			idfail = 0;
errin:
			if ((paniced || yyshifts != 0) && yyrecover(ps, idfail)) {
				paniced = 0;
				ps = Ps;
				yystate = *ps;
				goto newstate;
			}
			/*
			 * Find a state where 'error' is a
			 * legal shift action.
			 */
			if (paniced && yyshifts <= 0 && ps >= panicps) {
				yypv =- (ps - panicps) + 1;
#ifdef PXP
				yypw =- (ps - panicps) + 1;
#endif
				ps = panicps - 1;
			}
			while (ps >= yys) {
				for (p = &yyact[ yypact[*ps+1] ] ; *p <= 0; p=+ 2)
					if (*p == -256) {
						panicps = ps;
						yystate= p[1] & 07777;
						yyOshifts = yyshifts;
						yyshifts = 0;
						paniced = 1;
						goto stack;
					}
				--ps;
				--yypv;
#ifdef PXP
				--yypw;
#endif
#ifdef PI
				if (OY.Yychar != YID)
					syneflg++;
#endif
				OY.Yychar = -1;
			}
			if (yychar == YEOF)
				yyunexeof();
			if (yystate == 1)
				yyexeof();
			yerror("Unrecoverable syntax error - QUIT");
			return;
	}
	panic("yyparse");
}
