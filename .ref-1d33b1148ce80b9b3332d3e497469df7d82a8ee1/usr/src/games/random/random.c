/*
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Guy Harris at Network Appliance Corp.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)random.c	8.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	struct timeval tp;
	double denom;
	int ch, random_exit, selected, unbuffer_output;
	char *ep;

	random_exit = unbuffer_output = 0;
	while ((ch = getopt(argc, argv, "er")) != EOF)
		switch (ch) {
		case 'e':
			random_exit = 1;
			break;
		case 'r':
			unbuffer_output = 1;
			break;
		default:
		case '?':
			usage();
			/* NOTREACHED */
		}

	argc -= optind;
	argv += optind;

	switch (argc) {
	case 0:
		denom = 2;
		break;
	case 1:
		errno = 0;
		denom = strtod(*argv, &ep);
		if (errno == ERANGE)
			err(1, "%s", *argv);
		if (denom == 0 || *ep != '\0')
			errx(1, "denominator is not valid.");
		break;
	default:
		usage(); 
		/* NOTREACHED */
	}

	(void)gettimeofday(&tp, NULL);
	srandom((u_int)(tp.tv_usec + tp.tv_sec + getpid()));

	/* Compute a random exit status between 0 and denom - 1. */
	if (random_exit)
		return ((denom * random()) / LONG_MAX);

	/*
	 * Act as a filter, randomly choosing lines of the standard input
	 * to write to the standard output.
	 */
	if (unbuffer_output)
		setbuf(stdout, NULL);
	
	/*
	 * Select whether to print the first line.  (Prime the pump.)
	 * We find a random number between 0 and denom - 1 and, if it's
	 * 0 (which has a 1 / denom chance of being true), we select the
	 * line.
	 */
	selected = (int)(denom * random() / LONG_MAX) == 0;
	while ((ch = getchar()) != EOF) {
		if (selected)
			(void)putchar(ch);
		if (ch == '\n') {
			/* End of that line.  See if we got an error. */
			if (ferror(stdout))
				err(2, "stdout");

			/* Now see if the next line is to be printed. */
			selected = (int)(denom * random() / LONG_MAX) == 0;
		}
	}
	if (ferror(stdin))
		err(2, "stdin");
	exit (0);
}

void
usage()
{

	(void)fprintf(stderr, "usage: random [-er] [denominator]\n");
	exit(1);
}
