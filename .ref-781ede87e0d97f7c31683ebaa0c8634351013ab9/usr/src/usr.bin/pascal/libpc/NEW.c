/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)NEW.c	1.4 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

NEW(var, size)
	char	**var;	/* pointer to item being deallocated */
	long	size;	/* sizeof struct pointed to by var */
{
	extern	char *malloc();
	char 	*memblk;

	memblk = malloc((int)size);
	if (memblk == 0) {
		ERROR("Ran out of memory\n", 0);
		return;
	}
	*var = memblk;
	if (memblk < _minptr)
		_minptr = memblk;
	if (memblk + size > _maxptr)
		_maxptr = memblk + size;
}
