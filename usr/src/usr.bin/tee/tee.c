/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1985 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char *sccsid = "@(#)tee.c	5.3 (Berkeley) %G%";
#endif
/*
 * tee-- pipe fitting
 */

#include <stdio.h>
#include <signal.h>

main(argc,argv)
	int argc;
	char *argv[];
{
	register FILE **openf, **lastf, **fdp;
	register i, cc;
	char buf[8192], *calloc();
	int aflag = 0;

	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		switch (argv[0][1]) {
		case 'a':
			aflag++;
			break;
		case 'i':
		case '\0':
			signal(SIGINT, SIG_IGN);
			break;
		}
		argv++, argc--;
	}
	lastf = openf = (FILE **)calloc(argc+1, sizeof (FILE *));
	if (openf == 0) {
		fprintf(stderr, "tee: Out of memory.\n");
		exit(-1);
	}
	*lastf++ = stdout;	/* default */
	for (; argc > 0; argc--, argv++) {
		*lastf = fopen(argv[0], aflag ? "a" : "w");
		if (*lastf == NULL)
			fprintf(stderr, "tee: %s: cannot open.\n", argv[0]);
		else
			lastf++;
	}
	while ((cc = read(fileno(stdin), buf, sizeof (buf))) > 0)
		for (fdp = openf; fdp < lastf; fdp++)
			fwrite(buf, 1, cc, *fdp);
	exit(0);
}
