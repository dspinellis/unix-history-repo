/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)NAM.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

char *
NAM(value, name)

	register int	value;	/* internal enumerated type value */
	char		*name;	/* ptr to enumerated type name descriptor */
{
	register short	*sptr;

	sptr = (short *)name;
	if (value < 0 || value >= *sptr) {
		ERROR(ENAMRNG, value);
		return;
	}
	sptr++;
	return	name + 2 + sptr[value];
}
