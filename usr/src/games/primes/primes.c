/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)primes.c	5.1 (Wollongong) %G%";
#endif not lint

/*
 *	primes [ number ]
 *
 *	Print all primes greater than argument (or number read from stdin).
 *
 *	A free translation of 'primes.s'
 *
 */

#include <stdio.h>
#include <math.h>

#define	TABSIZE	1000		/* size of sieve table */
#define	BIG	4294967296.	/* largest unsigned int */

char	table[TABSIZE];		/* table for sieve of Eratosthenes */
int	tabbits	= 8*TABSIZE;	/* number of bits in table */

float	fstart;
unsigned	start;			/* lowest number to test for prime */
char	bittab[] = {		/* bit positions (to save shifting) */
	01, 02, 04, 010, 020, 040, 0100, 0200
};

unsigned pt[] =	{		/* primes < 100 */
	2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
	47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
};

unsigned factab[] = {		/* difference between succesive trial factors */
	10, 2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4,
	2, 6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4,
	6, 2, 6, 6, 4, 2, 4, 6, 2, 6, 4, 2, 4, 2, 10, 2
};

main(argc, argv)
int	argc;
char	**argv;
{
	register unsigned	*fp;
	register char	*p;
	register int	i;
	unsigned	quot;
	unsigned	factor, v;

	if (argc >= 2) {		/* get starting no. from arg */
		if (sscanf(argv[1], "%f", &fstart) != 1
		    || fstart < 0.0 || fstart >= BIG) {
			ouch();
			exit(1);
		}
	} else {			/* get starting no. from stdin */
		while ((i = scanf("%f", &fstart)) != 1
		    || fstart < 0.0 || fstart >= BIG) {
			if (i == EOF)
				exit(1);
			ouch();
		}
	}
	start = (unsigned)fstart;

	/*
	 * Quick list of primes < 100
	 */
	if (start <= 97) {
		for (fp = pt; *fp < start; fp++)
			;
		do
			printf("%u\n", *fp);
		while (++fp < &pt[sizeof(pt) / sizeof(*pt)]);
		start = 100;
	}
	quot = start/2;
	start = quot * 2 + 1;

/*
 * Loop forever:
 */
    for (;;) {
	/*
	 * Generate primes via sieve of Eratosthenes
	 */
	for (p = table; p < &table[TABSIZE]; p++)	/* clear sieve */
		*p = '\0';
	v = (unsigned)sqrt((float)(start + tabbits)); /* highest useful factor */
	sieve(3);
	sieve(5);
	sieve(7);
	factor = 11;
	fp = &factab[1];
	do {
		sieve(factor);
		factor += *fp;
		if (++fp >= &factab[sizeof(factab) / sizeof(*factab)])
			fp = factab;
	} while (factor <= v);
	/*
	 * Print generated primes
	 */
	for (i = 0; i < 8*TABSIZE; i += 2) {
		if ((table[i>>3] & bittab[i&07]) == 0)
			printf("%u\n", start);
		start += 2;
	}
    }
}

/*
 * Insert all multiples of given factor into the sieve
 */
sieve(factor)
unsigned factor;
{
	register int	i;
	unsigned	off;
	unsigned	quot;

	quot = start / factor;
	off = (quot * factor) - start;
	if ((int)off < 0)
		off += factor;
	while (off < tabbits ) {
		i = (int)off;
		table[i>>3] |= bittab[i&07];
		off += factor;
	}
}

/*
 * Error message
 */
ouch()
{
	fprintf(stderr, "Ouch.\n");
}
