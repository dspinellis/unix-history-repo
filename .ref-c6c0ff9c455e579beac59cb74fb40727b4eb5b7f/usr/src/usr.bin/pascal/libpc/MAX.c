/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)MAX.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

MAX(width, reduce, min)

	register int	width;		/* requested width */
	int		reduce;		/* amount of extra space required */
	int		min;		/* minimum amount of space needed */
{
	if (width < 0) {
		ERROR(EFMTSIZE, width);
		return;
	}
	if ((width -= reduce) >= min)
		return width;
	return min;
}
