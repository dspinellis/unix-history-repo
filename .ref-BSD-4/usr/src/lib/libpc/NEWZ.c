/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)NEWZ.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

NEWZ(var, size)
	char	**var;	/* pointer to item being deallocated */
	int	size;	/* sizeof struct pointed to by var */
{
	register char	*cp;
	char		*limit;
	extern char	*malloc();

	cp = malloc(size);
	if (cp == 0) {
		ERROR(EOUTOFMEM,0);
		return;
	}
	*var = cp;
	if (cp < _minptr)
		_minptr = cp;
	limit = cp + size;
	if (limit > _maxptr)
		_maxptr = limit;
	for (; cp < limit; *cp++ = '\0')
		/* void */;
}
