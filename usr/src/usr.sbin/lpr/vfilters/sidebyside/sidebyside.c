/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
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
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)sidebyside.c	1.5 (Berkeley) 6/1/90";
#endif /* not lint */

#include <stdio.h>
/*
 * sidebyside -- make wide listings by placing pages side by side
 */
int	width = 90;

#define	LINELN 440
#define	EJLINE	86
#define	LMARG	10

char	screen[EJLINE][LINELN];
char	ul[EJLINE][LINELN];
char	anyul[EJLINE];
int	frame;
int	origin;
int	outline;
int	outcol;

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		switch (argv[0][1]) {

		case 'w':
			width = atoi(argv[0]+2);
			break;
		
		default:
			fprintf(stderr, "usage: sidebyside [ -wwidth ] file ...\n");
			break;
		}
		argc--, argv++;
	}
	clear(screen, EJLINE * LINELN);
	origin = LMARG;
	outcol = LMARG;
	cutmark(LMARG);
	do {
		if (argc > 0) {
			if (freopen(argv[0], "r", stdin) == NULL) {
				perror(argv[0]);
				argc--, argv++;
				continue;
			}
			argc--, argv++;
		}
		process();
	} while (argc > 0);
	exit(0);
}

process()
{
	char linebuf[BUFSIZ];
	register char *cp;
	register int c;

	while (fgets(linebuf, sizeof linebuf, stdin)) {
		for (cp = linebuf; c = *cp; cp++) switch (c) {

		case '\t':
			do {
				int ooutcol = outcol;
				outchar(' ');
				if (outcol == ooutcol)
					break;
			} while ((outcol - origin) % 8 != 0);
			break;

		case '\b':
			if (outcol > origin)
				outcol--;
			break;

		case '\r':
			outcol = origin + LMARG; 
			break;

		case '\f':
			outline = EJLINE - 1;
			/* fall into ... */

		case '\n':
			outline++;
			if (outline == EJLINE) {
				origin += width;
				if (origin + width > LINELN) {
					cutmark(origin);
					oflush();
					break;
				}
/*
				if (origin * 2 + LMARG < LINELN && origin * 3 > LINELN) {
					cutmark(origin);
					origin += LMARG;
				}
*/
				outline = 0;
				cutmark(origin);
			}
			outcol = origin;
			break;

		default:
			outchar(c);
			break;
		}
	}
	if (outline || origin != LMARG) {
		cutmark(origin + width);
		oflush();
	}
}

outchar(c)
	register int c;
{
	register char *cp = screen[outline];
	register char *up;
	register int d;

	if (c < 040 || c >= 0177)
		return;
	if (outcol < LINELN) {
		cp += outcol;
		d = *cp;
		if (d == ' ') {
			*cp = c;
			outcol++;
			return;
		}
		if (d == '_' || c == '_') {
			if (c == d) {
				outcol++;
				return;
			}
			if (anyul[outline] == 0)
				clear(ul[outline], LINELN);
			anyul[outline] = 1;
			ul[outline][outcol] = '_';
			if (c == '_')
				c = d;
		}
		*cp = c;
		outcol++;
	}
}

oflush()
{
	register char *cp, *dp;
	register int i, j, oc, dc, c;

	frame++;
/*
	if (frame > 1) {
		printf("\n\n\n");
		for (j = 0; j < LINELN; j++)
			putchar('_');
		printf("\n");
	}
*/
	printf("\n");
	for (i = 0; i < EJLINE; i++) {
		putline(screen[i]);
		if (anyul[i]) {
			putchar('\r');
			putline(ul[i]);
			anyul[i] = 0;
		}
		putchar('\n');
	}
	for (i = 0; i < LINELN; i++)
		putchar('_');
	putchar('\n');
	clear(screen, EJLINE * LINELN);
	outline = 0;
	outcol = LMARG;
	origin = LMARG;
	cutmark(LMARG);
}

clear(cp, i)
	register char *cp;
	register int i;
{

	if (i > 0)
		do
			*cp++ = ' ';
		while (--i);
}

cutmark(o)
	register int o;
{
	register int i;

	screen[0][o - LMARG] = '|';
	screen[1][o - LMARG] = '|';
	screen[EJLINE - 1][o - LMARG] = '|';
	screen[EJLINE - 2][o - LMARG] = '|';
}

putline(cp)
	register char *cp;
{
	register int j = LINELN;
	register int c, oc, dc;

	oc = dc = 0;
	do {
		if ((c = *cp++) == ' ') {
			dc++;
			continue;
		}
		while (((oc + 8) &~ 7) < dc) {
			putchar('\t');
			oc = (oc + 8) &~ 7;
		}
		while (oc < dc) {
			putchar(' ');
			oc++;
		}
		putchar(c);
		oc++, dc++;
	} while (--j != 0);
}
