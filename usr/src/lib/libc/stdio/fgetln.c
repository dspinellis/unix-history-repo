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
static char sccsid[] = "@(#)fgetln.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "local.h"

/*
 * Expand the line buffer.  Return -1 on error.
 * The `new size' does not account for a terminating '\0',
 * so we add 1 here.
 */
__slbexpand(fp, newsize)
	FILE *fp;
	size_t newsize;
{
	void *p;

	if (fp->_lb._size >= ++newsize)
		return (0);
	if ((p = realloc(fp->_lb._base, newsize)) == NULL)
		return (-1);
	fp->_lb._base = p;
	fp->_lb._size = newsize;
	return (0);
}

/*
 * Get an input line.  The returned pointer often (but not always)
 * points into a stdio buffer.  Fgetline smashes the newline (if any)
 * in the stdio buffer; callers must not use it on streams that
 * have `magic' setvbuf() games happening.
 */
char *
fgetline(fp, lenp)
	register FILE *fp;
	size_t *lenp;
{
	register unsigned char *p;
	register size_t len;
	size_t off;

	/* make sure there is input */
	if (fp->_r <= 0 && __srefill(fp)) {
		if (lenp != NULL)
			*lenp = 0;
		return (NULL);
	}

	/* look for a newline in the input */
	if ((p = memchr((void *)fp->_p, '\n', fp->_r)) != NULL) {
		register char *ret;

		/*
		 * Found one.  Flag buffer as modified to keep
		 * fseek from `optimising' a backward seek, since
		 * the newline is about to be trashed.  (We should
		 * be able to get away with doing this only if
		 * p is not pointing into an ungetc buffer, since
		 * fseek discards ungetc data, but this is the
		 * usual case anyway.)
		 */
		ret = (char *)fp->_p;
		len = p - fp->_p;
		fp->_flags |= __SMOD;
		*p = 0;
		fp->_r -= len + 1;
		fp->_p = p + 1;
		if (lenp != NULL)
			*lenp = len;
		return (ret);
	}

	/*
	 * We have to copy the current buffered data to the line buffer.
	 *
	 * OPTIMISTIC is length that we (optimistically)
	 * expect will accomodate the `rest' of the string,
	 * on each trip through the loop below.
	 */
#define OPTIMISTIC 80

	for (len = fp->_r, off = 0;; len += fp->_r) {
		register size_t diff;

		/*
		 * Make sure there is room for more bytes.
		 * Copy data from file buffer to line buffer,
		 * refill file and look for newline.  The
		 * loop stops only when we find a newline.
		 */
		if (__slbexpand(fp, len + OPTIMISTIC))
			goto error;
		(void) bcopy((void *)fp->_p, (void *)(fp->_lb._base + off),
		    len - off);
		off = len;
		if (__srefill(fp))
			break;	/* EOF or error: return partial line */
		if ((p = memchr((void *)fp->_p, '\n', fp->_r)) == NULL)
			continue;

		/* got it: finish up the line (like code above) */
		fp->_flags |= __SMOD;	/* soon */
		diff = p - fp->_p;
		len += diff;
		if (__slbexpand(fp, len))
			goto error;
		(void) bcopy((void *)fp->_p, (void *)(fp->_lb._base + off),
		    diff);
		fp->_r -= diff + 1;
		fp->_p = p + 1;
		break;
	}
	if (lenp != NULL)
		*lenp = len;
	fp->_lb._base[len] = 0;
	return ((char *)fp->_lb._base);

error:
	if (lenp != NULL)
		*lenp = 0;	/* ??? */
	return (NULL);		/* ??? */
}
