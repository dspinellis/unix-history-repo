/*-
 * Copyright (c) 1992, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tscroll.c	8.4 (Berkeley) %G%";
#endif /* not lint */

#include "curses.h"

#define	MAXRETURNSIZE	64

/*
 * Routine to perform scrolling.  Derived from tgoto.c in tercamp(3)
 * library.  Cap is a string containing printf type escapes to allow
 * scrolling.  The following escapes are defined for substituting n:
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
__tscroll(cap, n1, n2)
	const char *cap;
	int n1, n2;
{
	static char result[MAXRETURNSIZE];
	int c, n;
	char *dp;

	if (cap == NULL)
		goto err;
	for (n = n1, dp = result; (c = *cap++) != '\0';) {
		if (c != '%') {
			*dp++ = c;
			continue;
		}
		switch (c = *cap++) {
		case 'n':
			n ^= 0140;
			continue;
		case 'd':
			if (n < 10)
				goto one;
			if (n < 100)
				goto two;
			/* FALLTHROUGH */
		case '3':
			*dp++ = (n / 100) | '0';
			n %= 100;
			/* FALLTHROUGH */
		case '2':
two:			*dp++ = n / 10 | '0';
one:			*dp++ = n % 10 | '0';
			n = n2;
			continue;
		case '>':
			if (n > *cap++)
				n += *cap++;
			else
				cap++;
			continue;
		case '+':
			n += *cap++;
			/* FALLTHROUGH */
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
		/*
		 * XXX
		 * System V terminfo files have lots of extra gunk.
		 * The only one we've seen in scrolling strings is
		 * %pN, and it seems to work okay if we ignore it.
		 */
		case 'p':
			++cap;
			continue;
		default:
			goto err;
		}
	}
	*dp = '\0';
	return (result);

err:	return("curses: __tscroll failed");
}
