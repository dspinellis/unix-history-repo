/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fgetln.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "local.h"

/*
 * Expand the line buffer.  Return -1 on error.
#ifdef notdef
 * The `new size' does not account for a terminating '\0',
 * so we add 1 here.
#endif
 */
__slbexpand(fp, newsize)
	FILE *fp;
	size_t newsize;
{
	void *p;

#ifdef notdef
	++newsize;
#endif
	if (fp->_lb._size >= newsize)
		return (0);
	if ((p = realloc(fp->_lb._base, newsize)) == NULL)
		return (-1);
	fp->_lb._base = p;
	fp->_lb._size = newsize;
	return (0);
}

/*
 * Get an input line.  The returned pointer often (but not always)
 * points into a stdio buffer.  Fgetline does not alter the text of
 * the returned line (which is thus not a C string because it will
 * not necessarily end with '\0'), but does allow callers to modify
 * it if they wish.  Thus, we set __SMOD in case the caller does.
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
		*lenp = 0;
		return (NULL);
	}

	/* look for a newline in the input */
	if ((p = memchr((void *)fp->_p, '\n', fp->_r)) != NULL) {
		register char *ret;

		/*
		 * Found one.  Flag buffer as modified to keep fseek from
		 * `optimising' a backward seek, in case the user stomps on
		 * the text.
		 */
		p++;		/* advance over it */
		ret = (char *)fp->_p;
		*lenp = len = p - fp->_p;
		fp->_flags |= __SMOD;
		fp->_r -= len;
		fp->_p = p;
		return (ret);
	}

	/*
	 * We have to copy the current buffered data to the line buffer.
	 * As a bonus, though, we can leave off the __SMOD.
	 *
	 * OPTIMISTIC is length that we (optimistically) expect will
	 * accomodate the `rest' of the string, on each trip through the
	 * loop below.
	 */
#define OPTIMISTIC 80

	for (len = fp->_r, off = 0;; len += fp->_r) {
		register size_t diff;

		/*
		 * Make sure there is room for more bytes.  Copy data from
		 * file buffer to line buffer, refill file and look for
		 * newline.  The loop stops only when we find a newline.
		 */
		if (__slbexpand(fp, len + OPTIMISTIC))
			goto error;
		(void)memcpy((void *)(fp->_lb._base + off), (void *)fp->_p,
		    len - off);
		off = len;
		if (__srefill(fp))
			break;	/* EOF or error: return partial line */
		if ((p = memchr((void *)fp->_p, '\n', fp->_r)) == NULL)
			continue;

		/* got it: finish up the line (like code above) */
		p++;
		diff = p - fp->_p;
		len += diff;
		if (__slbexpand(fp, len))
			goto error;
		(void)memcpy((void *)(fp->_lb._base + off), (void *)fp->_p,
		    diff);
		fp->_r -= diff;
		fp->_p = p;
		break;
	}
	*lenp = len;
#ifdef notdef
	fp->_lb._base[len] = 0;
#endif
	return ((char *)fp->_lb._base);

error:
	*lenp = 0;		/* ??? */
	return (NULL);		/* ??? */
}
