/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)yyerror.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "whoami.h"
#include "0.h"
#include "yy.h"

/*
 * Yerror prints an error
 * message and then returns
 * NIL for the tree if needed.
 * The error is flagged on the
 * current line which is printed
 * if the listing is turned off.
#ifdef PXP
 *
 * As is obvious from the fooling around
 * with fout below, the Pascal system should
 * be changed to use the new library "lS".
#endif
 */
yerror(s, a1, a2, a3, a4, a5)
	char *s;
{
#ifdef PI
	char buf[256];
#endif
	register int i, j;
	static yySerrs;
#ifdef PXP
/*
	int ofout;
*/
#endif

	if (errpfx == 'w' && opt('w') != 0)
		return;
#ifdef PXP
/*
	flush();
	ofout = fout[0];
	fout[0] = errout;
*/
#endif
	yyResume = 0;
#ifdef PI
	geterr(s, buf);
	s = buf;
#endif
	yysync();
	putc(errpfx, stderr);
	putc(' ', stderr);
	for (i = 3; i < yyecol; i++)
		putc('-', stderr);
	fprintf(stderr, "^--- ");
/*
	if (yyecol > 60)
		printf("\n\t");
*/
	fprintf(stderr, s, a1, a2, a3, a4, a5);
	putc('\n', stderr);
	if (errpfx == 'E')
#ifdef PI
		eflg++, cgenflg++;
#endif
#ifdef PXP
		eflg++;
#endif
	errpfx = 'E';
	yySerrs++;
	if (yySerrs >= MAXSYNERR) {
		yySerrs = 0;
		yerror("Too many syntax errors - QUIT");
		pexit(ERRS);
	}
#ifdef PXP
/*
	flush();
	fout[0] = ofout;
*/
	return (0);
#endif
}

/*
 * A bracketing error message
 */
brerror(where, what)
	int where;
	char *what;
{

	if (where == 0) {
		line = yyeline;
		setpfx(' ');
		error("End matched %s on line %d", what, where);
		return;
	}
	if (where < 0)
		where = -where;
	yerror("Inserted keyword end matching %s on line %d", what, where);
}
