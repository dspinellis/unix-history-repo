/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Landon Curt Noll.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)factor.c	8.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * factor - factor a number into primes
 *
 * By: Landon Curt Noll   chongo@toad.com,   ...!{sun,tolsoft}!hoptoad!chongo
 *
 *   chongo <for a good prime call: 391581 * 2^216193 - 1> /\oo/\
 *
 * usage:
 *	factor [number] ...
 *
 * The form of the output is:
 *
 *	number: factor1 factor1 factor2 factor3 factor3 factor3 ...
 *
 * where factor1 < factor2 < factor3 < ...
 *
 * If no args are given, the list of numbers are read from stdin.
 */

#include <err.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "primes.h"

/*
 * prime[i] is the (i-1)th prime.
 *
 * We are able to sieve 2^32-1 because this byte table yields all primes 
 * up to 65537 and 65537^2 > 2^32-1.
 */
extern ubig prime[];
extern ubig *pr_limit;		/* largest prime in the prime array */

void	pr_fact __P((ubig));	/* print factors of a value */
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	ubig val;
	int ch;
	char *p, buf[100];		/* > max number of digits. */

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch (ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/* No args supplied, read numbers from stdin. */
	if (argc == 0)
		for (;;) {
			if (fgets(buf, sizeof(buf), stdin) == NULL) {
				if (ferror(stdin))
					err(1, "stdin");
				exit (0);
			}
			for (p = buf; isblank(*p); ++p);
			if (*p == '\n' || *p == '\0')
				continue;
			if (*p == '-')
				errx(1, "negative numbers aren't permitted.");
			errno = 0;
			val = strtoul(buf, &p, 10);
			if (errno)
				err(1, "%s", buf);
			if (*p != '\n')
				errx(1, "%s: illegal numeric format.", buf);
			pr_fact(val);
		}
	/* Factor the arguments. */
	else
		for (; *argv != NULL; ++argv) {
			if (argv[0][0] == '-')
				errx(1, "negative numbers aren't permitted.");
			errno = 0;
			val = strtoul(argv[0], &p, 10);
			if (errno)
				err(1, "%s", argv[0]);
			if (*p != '\0')
				errx(1, "%s: illegal numeric format.", argv[0]);
			pr_fact(val);
		}
	exit(0);
}

/*
 * pr_fact - print the factors of a number
 *
 * If the number is 0 or 1, then print the number and return.
 * If the number is < 0, print -1, negate the number and continue
 * processing.
 *
 * Print the factors of the number, from the lowest to the highest.
 * A factor will be printed numtiple times if it divides the value
 * multiple times.
 *
 * Factors are printed with leading tabs.
 */
void
pr_fact(val)
	ubig val;		/* Factor this value. */
{
	ubig *fact;		/* The factor found. */

	/* Firewall - catch 0 and 1. */
	if (val == 0)		/* Historical practice; 0 just exits. */
		exit(0);
	if (val == 1) {
		(void)printf("1: 1\n");
		return;
	}

	/* Factor value. */
	(void)printf("%ld:", val);
	for (fact = &prime[0]; val > 1; ++fact) {
		/* Look for the smallest factor. */
		do {
			if (val % (long)*fact == 0)
				break;
		} while (++fact <= pr_limit);

		/* Watch for primes larger than the table. */
		if (fact > pr_limit) {
			(void)printf(" %ld", val);
			break;
		}

		/* Divide factor out until none are left. */
		do {
			(void)printf(" %ld", *fact);
			val /= (long)*fact;
		} while ((val % (long)*fact) == 0);

		/* Let the user know we're doing something. */
		(void)fflush(stdout);
	}
	(void)putchar('\n');
}

void
usage()
{
	(void)fprintf(stderr, "usage: factor [value ...]\n");
	exit (0);
}
