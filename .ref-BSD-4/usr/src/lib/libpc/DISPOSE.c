/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)DISPOSE.c 1.1 10/29/80";

#include	"h00vars.h"
#include	"h01errs.h"

DISPOSE(var, size)
	register char	**var;	/* pointer to pointer being deallocated */
	int		size;	/* sizeof(bletch) */
{
	if (*var == 0 || *var + size > _maxptr || *var < _minptr) {
		ERROR(ENILPTR,0);
		return;
	}
	free(*var);
	if (*var == _minptr)
		_minptr += size;
	if (*var + size == _maxptr)
		_maxptr -= size;
	*var = (char *)(0);
}
