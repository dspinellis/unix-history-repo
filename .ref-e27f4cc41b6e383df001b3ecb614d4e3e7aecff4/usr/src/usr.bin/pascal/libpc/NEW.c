/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)NEW.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

NEW(var, size)
	char	**var;	/* pointer to item being deallocated */
	int	size;	/* sizeof struct pointed to by var */
{
	extern	char *malloc();
	char 	*memblk;

	memblk = malloc(size);
	if (memblk == 0) {
		ERROR(EOUTOFMEM,0);
		return;
	}
	*var = memblk;
	if (memblk < _minptr)
		_minptr = memblk;
	if (memblk + size > _maxptr)
		_maxptr = memblk + size;
}
