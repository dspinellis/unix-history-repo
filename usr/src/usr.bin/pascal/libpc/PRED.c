/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PRED.c 1.2 %G%";

#include "h01errs.h"

long
PRED(value, lower, upper)

	long	value;
	long	lower;
	long	upper;
{
	value--;
	if (value < lower || value > upper) {
		ERROR(ERANGE, value);
		return;
	}
	return	value;
}
