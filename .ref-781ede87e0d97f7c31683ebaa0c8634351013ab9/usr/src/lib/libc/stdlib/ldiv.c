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
static char sccsid[] = "@(#)ldiv.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>		/* ldiv_t */

/*
 * I AM NOT SURE THIS IS COMPLETELY PORTABLE
 * (or that it is even right)
 */
ldiv_t
ldiv(num, denom)
	long num, denom;
{
	ldiv_t r;

	/* see div.c for comments */

	if (num > 0 && denom < 0) {
		num = -num;
		denom = -denom;
	}
	r.quot = num / denom;
	r.rem = num % denom;
	if (num < 0 && denom > 0) {
		if (r.rem > 0) {
			r.quot++;
			r.rem -= denom;
		}
	}
	return (r);
}
