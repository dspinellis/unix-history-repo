/*
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ldiv.c	5.1 (Berkeley) 5/16/90";
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
