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
static char sccsid[] = "@(#)fread.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <string.h>

fread(buf, size, count, fp)
	void *buf;
	size_t size, count;
	register FILE *fp;
{
	register size_t resid;
	register char *p;
	register int r;
	size_t total;

	if ((resid = count * size) == 0)
		return (count);
	if (fp->_r < 0)
		fp->_r = 0;
	total = resid;
	p = buf;
	while (resid > (r = fp->_r)) {
		(void) bcopy((void *)fp->_p, (void *)p, (size_t)r);
		fp->_p += r;
		/* fp->_r = 0 ... done in __srefill */
		p += r;
		resid -= r;
		if (__srefill(fp)) {
			/* no more input: return partial result */
			return ((total - resid) / size);
		}
	}
	(void) bcopy((void *)fp->_p, (void *)p, resid);
	fp->_r -= resid;
	fp->_p += resid;
	return (count);
}
