/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jeffrey Mogul.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)swab.c	5.10 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>

void
swab(from, to, n)
	const void *from;
	void *to;
	register size_t n;
{
	register char *fp, *tp;
	register unsigned long temp;

	n >>= 1; n++;
	fp = (char *)from;
	tp = (char *)to;
#define	STEP	temp = *fp++,*tp++ = *fp++,*tp++ = temp
	/* round to multiple of 8 */
	while ((--n) & 07)
		STEP;
	n >>= 3;
	while (--n >= 0) {
		STEP; STEP; STEP; STEP;
		STEP; STEP; STEP; STEP;
	}
}
