/*
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ldiv.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>		/* ldiv_t */

ldiv_t
ldiv(num, denom)
	long num, denom;
{
	ldiv_t r;

	/* see div.c for comments */

	r.quot = num / denom;
	r.rem = num % denom;
	if (num >= 0 && r.rem < 0) {
		r.quot++;
		r.rem -= denom;
	}
	return (r);
}
