/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)MAX.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

long
MAX(width, reduce, min)

	register long	width;		/* requested width */
	long		reduce;		/* amount of extra space required */
	long		min;		/* minimum amount of space needed */
{
	if (width <= 0) {
		ERROR("Non-positive format width: %D\n", width);
	}
	if ((width -= reduce) >= min)
		return width;
	return min;
}
