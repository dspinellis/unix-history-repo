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
static char sccsid[] = "@(#)strxfrm.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/cdefs.h>
#include <string.h>

/*
 * Transform src, storing the result in dst, such that
 * strcmp() on transformed strings returns what strcoll()
 * on the original untransformed strings would return.
 */
size_t
strxfrm(dst, src, n)
	register char *dst;
	const char *src;
	size_t n;
{
	register size_t srclen, copysize;

	/*
	 * Since locales are unimplemented, this is just a copy.
	 */
	srclen = strlen(src);
	if (n != 0) {
		copysize = srclen < n ? srclen : n - 1;
		(void)memcpy(dst, src, copysize);
		dst[copysize] = 0;
	}
	return (srclen);
}
