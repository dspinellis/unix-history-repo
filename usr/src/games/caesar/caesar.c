/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Adams.
 *
 * Authors:
 *	Stan King, John Eldridge, based on algorithm suggested by
 *	Bob Morris
 * 29-Sep-82
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)caesar.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <math.h>
#include <stdio.h>
#include <ctype.h>

#define	LINELENGTH	2048
#define	ROTATE(ch, perm) \
	isupper(ch) ? ('A' + (ch - 'A' + perm % 26)) : \
	    islower(ch) ? ('a' + (ch - 'a' + perm) % 26) : ch

main(argc, argv)
	int argc;
	char **argv;
{
	/*
	 * letter frequencies (taken from some unix(tm) documentation)
	 * (unix is a trademark of Bell Laboratories)
	 */
	static double stdf[26] = {
		7.97, 1.35, 3.61, 4.78, 12.37, 2.01, 1.46, 4.49, 6.39, 0.04,
		0.42, 3.81, 2.69, 5.92,  6.96, 2.91, 0.08, 6.63, 8.77, 9.68,
		2.62, 0.81, 1.88, 0.23,  2.07, 0.06,
	};
	double dot, winnerdot;
	register int i, ch;
	register char *inbuf;
	int bufsize, obs[26], rot, try, winner;
	char *malloc();

	if (argc > 1) {
		if ((rot = atoi(argv[1])) < 0) {
			(void)fprintf(stderr, "caesar: bad rotation value.\n");
			exit(1);
		}
		printit(rot);
		exit(0);
	}

	if (!(inbuf = malloc(LINELENGTH))) {
		(void)fprintf(stderr, "caesar: out of memory.\n");
		exit(1);
	}

	/* adjust frequency table to weight low probs REAL low */
	for (i = 0; i < 26; ++i)
		stdf[i] = log(stdf[i]) + log(26.0 / 100.0);

	/* decode each line separately */
	for (bufsize = 0;;) {
		for (i = 0; i < 26; obs[i++] = 0);

		/* get a sample of the text */
		for (i = 0; i < LINELENGTH; i++) {
			if ((ch = getchar()) == EOF)
				exit(0);
			inbuf[i] = ch;
			if (ch == '\n')
				break;
			if (islower(ch))
				obs[ch - 'a'] += 1;
			else if (isupper(ch))
				obs[ch - 'A'] += 1;
		}
		bufsize = i + 1;

		/*
		 * now "dot" the freqs with the observed letter freqs
		 * and keep track of best fit
		 */
		for (try = winner = 0; try < 26; try += 13) {
			dot = 0;
			for (i = 0; i < 26; i++)
				dot += obs[i] * stdf[(i + try) % 26];
			/* initialize winning score */
			if (try == 0)
				winnerdot = dot;
			if (dot > winnerdot) {
				/* got a new winner! */
				winner = try;
				winnerdot = dot;
			}
		}

		/* print out sample buffer */
		while (bufsize--) {
			ch = *inbuf++;
			putchar(ROTATE(ch, winner));
		}
	}
}

printit(rot)
	register int rot;
{
	register int ch;

	while ((ch = getchar()) != EOF)
		putchar(ROTATE(ch, rot));
}
