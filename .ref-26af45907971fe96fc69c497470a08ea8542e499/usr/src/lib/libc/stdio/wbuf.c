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
static char sccsid[] = "@(#)wbuf.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include "local.h"

/*
 * Write the given character into the (probably full) buffer for
 * the given file.  Flush the buffer out if it is or becomes full,
 * or if c=='\n' and the file is line buffered.
 */
__swbuf(c, fp)
	register int c;
	register FILE *fp;
{
	register int n;

	/*
	 * In case we cannot write, or longjmp takes us out early,
	 * make sure _w is 0 (if fully- or un-buffered) or -_bf._size
	 * (if line buffered) so that we will get called again.
	 * If we did not do this, a sufficient number of putc()
	 * calls might wrap _w from negative to positive.
	 */
	fp->_w = fp->_lbfsize;
	if (cantwrite(fp))
		return (EOF);
	c = (unsigned char)c;

	/*
	 * If it is completely full, flush it out.  Then, in any case,
	 * stuff c into the buffer.  If this causes the buffer to fill
	 * completely, or if c is '\n' and the file is line buffered,
	 * flush it (perhaps a second time).  The second flush will always
	 * happen on unbuffered streams, where _bf._size==1; fflush()
	 * guarantees that putc() will always call wbuf() by setting _w
	 * to 0, so we need not do anything else.
	 */
	n = fp->_p - fp->_bf._base;
	if (n >= fp->_bf._size) {
		if (fflush(fp))
			return (EOF);
		n = 0;
	}
	fp->_w--;
	*fp->_p++ = c;
	if (++n == fp->_bf._size || (fp->_flags & __SLBF && c == '\n'))
		if (fflush(fp))
			return (EOF);
	return (c);
}
