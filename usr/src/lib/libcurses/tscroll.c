/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tscroll.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

#define	MAXRETURNSIZE	64

/*
 * Routine to perform scrolling.  Derived from tgoto.c in tercamp(3) library.
 * Cap is a string containing printf type escapes to allow
 * scrolling.
 * The following escapes are defined for substituting n:
 *
 *	%d	as in printf
 *	%2	like %2d
 *	%3	like %3d
 *	%.	gives %c hacking special case characters
 *	%+x	like %c but adding x first
 *
 *	The codes below affect the state but don't use up a value.
 *
 *	%>xy	if value > x add y
 *	%i	increments n
 *	%%	gives %
 *	%B	BCD (2 decimal digits encoded in one byte)
 *	%D	Delta Data (backwards bcd)
 *
 * all other characters are ``self-inserting''.
 */
char *
__tscroll(cap, n)
	const char *cap;
	int n;
{
	register char *dp;
	register int c;
	char *cp, result[MAXRETURNSIZE];

	if (cap == NULL) {
toohard:
		/*
		 * ``We don't do that under BOZO's big top''
		 */
		return ("OOPS");
	}

	cp = (char *) cap;
	dp = result;
	while (c = *cp++) {
		if (c != '%') {
			*dp++ = c;
			continue;
		}
		switch (c = *cp++) {
		case 'n':
			n ^= 0140;
			continue;
		case 'd':
			if (n < 10)
				goto one;
			if (n < 100)
				goto two;
			/* fall into... */
		case '3':
			*dp++ = (n / 100) | '0';
			n %= 100;
			/* fall into... */
		case '2':
two:	
			*dp++ = n / 10 | '0';
one:
			*dp++ = n % 10 | '0';
			continue;
		case '>':
			if (n > *cp++)
				n += *cp++;
			else
				cp++;
			continue;
		case '+':
			n += *cp++;
			/* fall into... */
		case '.':
			*dp++ = n;
			continue;
		case 'i':
			n++;
			continue;
		case '%':
			*dp++ = c;
			continue;

		case 'B':
			n = (n / 10 << 4) + n % 10;
			continue;
		case 'D':
			n = n - 2 * (n % 16);
			continue;
		default:
			goto toohard;
		}
	}
	*dp = '\0';
	return (result);
}
