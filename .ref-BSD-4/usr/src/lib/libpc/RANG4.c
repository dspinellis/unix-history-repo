/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RANG4.c 1.1 10/29/80";

#include "h01errs.h"

RANG4(value, lower, upper)

	int	value;
	int	lower;
	int	upper;
{
	if (value < lower || value > upper) {
		ERROR(ERANGE, value);
		return;
	}
	return	value;
}
