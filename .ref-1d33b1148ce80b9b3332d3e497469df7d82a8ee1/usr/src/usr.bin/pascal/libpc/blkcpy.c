/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)blkcpy.c	8.1 (Berkeley) %G%";
#endif /* not lint */

blkcpy(from, to, siz)
	register char	*from;
	register char	*to;
	long		siz;
{
	register int	size = siz;

	if (to < from)
		while(size-- > 0)
			*to++ = *from++;
	else {
		to += size;
		from += size;
		while(size-- > 0)
			*--to = *--from;
	}
}
