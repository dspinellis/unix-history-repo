/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)NIL.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

char *
NIL(ptr)

	char	*ptr;		/* pointer to struct */
{
	if (ptr > _maxptr || ptr < _minptr) {
		ERROR(ENILPTR, 0);
		return;
	}
	return ptr;
}
