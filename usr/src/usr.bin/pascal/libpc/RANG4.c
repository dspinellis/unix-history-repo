/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RANG4.c 1.2 %G%";

#include "h01errs.h"

long
RANG4(value, lower, upper)

	long	value;
	long	lower;
	long	upper;
{
	if (value < lower || value > upper) {
		ERROR(ERANGE, value);
		return;
	}
	return	value;
}
