/*
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)div.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>		/* div_t */

/*
 * I AM NOT SURE THIS IS COMPLETELY PORTABLE
 * (or that it is even right)
 */
div_t
div(num, denom)
	int num, denom;
{
	div_t r;

	/* avoid deep thought */
	if (num > 0 && denom < 0) {
		num = -num;
		denom = -denom;
	}
	r.quot = num / denom;
	r.rem = num % denom;
	if (num < 0 && denom > 0) {
		/*
		 * Machine division and remainer may work either way.  The
		 * ANSI standard says that |r.quot| < |n/d| (where n/d
		 * computed in infinite precision).  If the remainder is
		 * positive, we got the `wrong' answer, so fix it.
		 */
		if (r.rem > 0) {
			r.quot++;
			r.rem -= denom;
		}
	}
	return (r);
}
