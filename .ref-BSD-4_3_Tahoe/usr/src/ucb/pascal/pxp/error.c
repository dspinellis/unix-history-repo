/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)error.c	5.2 (Berkeley) 12/4/87";
#endif not lint

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

#ifdef PXP
extern	int yyline;
extern	char errout;
#endif

char	errpfx	= 'E';
extern	int yyline;
/*
 * Panic is called when impossible
 * (supposedly, anyways) situations
 * are encountered.
#ifdef PI
 * Panic messages should be short
 * as they do not go to the message
 * file.
#endif
 */
panic(s)
	char *s;
{

#ifdef DEBUG
	fprintf(stderr, "Snark (%s) line=%d yyline=%d\n", s, line, yyline);
#endif
#ifdef PXP
	Perror( "Snark in pxp", s);
#endif
#ifdef PI
	Perror( "Snark in pi", s);
#endif
	pexit(DIED);
}

extern	char *errfile;
/*
 * Error is called for
 * semantic errors and
 * prints the error and
 * a line number.
 */
error(a1, a2, a3, a4)
{
#ifdef PI
	char buf[256];
	register int i;
#endif
#ifdef PXP
/*
	int ofout;
*/
#endif

	if (errpfx == 'w' && opt('w') != 0) {
		errpfx == 'E';
		return;
	}
#ifdef PXP
/*
	flush();
	ofout = fout[0];
	fout[0] = errout;
*/
#endif
#ifdef PI
	Enocascade = 0;
	geterr(a1, buf);
	a1 = buf;
#endif
	if (line < 0)
		line = -line;
	yySsync();
	yysetfile(filename);
#ifdef PI
	if (errpfx == ' ') {
		printf("  ");
		for (i = line; i >= 10; i /= 10)
			putchar(' ');
		printf("... ");
	} else if (Enoline)
		printf("  %c - ", errpfx);
	else
#endif
		fprintf(stderr, "%c %d - ", errpfx, line);
	fprintf(stderr, a1, a2, a3, a4);
	if (errpfx == 'E')
#ifdef PI
		eflg++, cgenflg++;
#endif
#ifdef PXP
		eflg++;
#endif
	errpfx = 'E';
#ifdef PI
	if (Eholdnl)
		Eholdnl = 0;
	else
#endif
		putc('\n', stderr);
#ifdef PXP
/*
	flush();
	fout[0] = ofout;
*/
#endif
}

#ifdef PI
cerror(a1, a2, a3, a4)
{

	if (Enocascade)
		return;
	setpfx(' ');
	error(a1, a2, a3, a4);
}
#endif
