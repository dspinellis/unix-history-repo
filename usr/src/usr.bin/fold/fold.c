/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)fold.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
/*
 * fold - fold long lines for finite output devices
 *
 * Bill Joy UCB June 28, 1977
 */

int	fold =  80;

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;
	FILE *f;

	argc--, argv++;
	if (argc > 0 && argv[0][0] == '-') {
		fold = 0;
		argv[0]++;
		while (*argv[0] >= '0' && *argv[0] <= '9')
			fold *= 10, fold += *argv[0]++ - '0';
		if (*argv[0]) {
			printf("Bad number for fold\n");
			exit(1);
		}
		argc--, argv++;
	}
	do {
		if (argc > 0) {
			if (freopen(argv[0], "r", stdin) == NULL) {
				perror(argv[0]);
				exit(1);
			}
			argc--, argv++;
		}
		for (;;) {
			c = getc(stdin);
			if (c == -1)
				break;
			putch(c);
		}
	} while (argc > 0);
	exit(0);
}

int	col;

putch(c)
	register c;
{
	register ncol;

	switch (c) {
		case '\n':
			ncol = 0;
			break;
		case '\t':
			ncol = (col + 8) &~ 7;
			break;
		case '\b':
			ncol = col ? col - 1 : 0;
			break;
		case '\r':
			ncol = 0;
			break;
		default:
			ncol = col + 1;
	}
	if (ncol > fold)
		putchar('\n'), col = 0;
	putchar(c);
	switch (c) {
		case '\n':
			col = 0;
			break;
		case '\t':
			col += 8;
			col &= ~7;
			break;
		case '\b':
			if (col)
				col--;
			break;
		case '\r':
			col = 0;
			break;
		default:
			col++;
			break;
	}
}
