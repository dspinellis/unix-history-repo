/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strxfrm.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/stdc.h>
#include <string.h>

/*
 * Transform src, storing the result in dst, such that
 * strcmp() on transformed strings returns what strcoll()
 * on the original untransformed strings would return.
 */
size_t
strxfrm(dst, src, n)
	register char *dst;
	register const char *src;
	register size_t n;
{
	register size_t r = 0;
	register int c;

	/*
	 * Since locales are unimplemented, this is just a copy.
	 */
	if (n != 0) {
		while ((c = *src++) != 0) {
			r++;
			if (--n == 0) {
				while (*src++ != 0)
					r++;
				break;
			}
			*dst++ = c;
		}
		*dst = 0;
	}
	return (r);
}
