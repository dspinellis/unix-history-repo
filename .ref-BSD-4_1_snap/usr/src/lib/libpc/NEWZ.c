/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)NEWZ.c 1.3 6/10/81";

#include "h00vars.h"

NEWZ(var, size)
	char	**var;	/* pointer to item being deallocated */
	long	size;	/* sizeof struct pointed to by var */
{
	register char	*cp;
	char		*limit;
	extern char	*malloc();

	cp = malloc((int)size);
	if (cp == 0) {
		ERROR("Ran out of memory\n", 0);
		return;
	}
	*var = cp;
	if (cp < _minptr)
		_minptr = cp;
	limit = cp + size;
	if (limit > _maxptr)
		_maxptr = limit;
	blkclr(size, cp);
}
