/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "yy.h"

/*
 * Yerror prints an error
 * message and then returns
 * NIL for the tree if needed.
 * The error is flagged on the
 * current line which is printed
 * if the listing is turned off.
 */
yerror(s, a1, a2, a3, a4, a5)
	register char *s;
{
	char buf[256];
	register int i, j;
	static yySerrs;

	if (errpfx == 'w' && opt('w') != 0)
		return;
	yyResume = 0;
	geterr(s, buf);
	s = buf;
	yysync();
	putchar(errpfx);
	putchar(' ');
	for (i = 3; i < yyecol; i++)
		putchar('-');
	printf("^--- ");
/*
	if (yyecol > 60)
		printf("\n\t");
*/
	printf(s, a1, a2, a3, a4, a5);
	putchar('\n');
	if (errpfx == 'E')
		eflg++;
	errpfx = 'E';
	yySerrs++;
	if (yySerrs >= MAXSYNERR) {
		yySerrs = 0;
		yerror("Too many syntax errors - QUIT");
		pexit(ERRS);
	}
}

/*
 * A bracketing error message
 */
brerror(where, what)
	register int where;
	register char *what;
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
