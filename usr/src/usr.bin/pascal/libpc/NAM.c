/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)NAM.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

char *
NAM(val, name)

	long		val;	/* internal enumerated type value */
	char		*name;	/* ptr to enumerated type name descriptor */
{
	register int	value = val;
	register short	*sptr;

	sptr = (short *)name;
	if (value < 0 || value >= *sptr) {
		ERROR(ENAMRNG, val);
		return;
	}
	sptr++;
	return	name + 2 + sptr[value];
}
