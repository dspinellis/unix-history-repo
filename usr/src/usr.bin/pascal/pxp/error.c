/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)error.c	8.1 (Berkeley) 6/6/93";
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
