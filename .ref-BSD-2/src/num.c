/* Copyright (c) 1979 Regents of the University of California */
#include "stdio.h"
/*
 * number - a cat like program which prints lines like the editor '#'' command
 *
 * Bill Joy UCB June 28, 1977
 */

int	lino;

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;
	register lastc;

	argc--, argv++;
	do {
		if (argc > 0) {
			if (freopen(argv[0], "r", stdin) == NULL) {
				perror(argv[0]);
				exit(1);
			}
			argc--, argv++;
		}
		lastc = '\n';
		for (;;) {
			c = getchar();
			if (c == -1)
				break;
			if (lastc == '\n')
				printf("%6d  ", ++lino);
			lastc = c;
			putchar(c);
		}
	} while (argc > 0);
	exit(0);
}
