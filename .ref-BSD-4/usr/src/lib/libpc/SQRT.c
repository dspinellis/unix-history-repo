/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SQRT.c 1.1 10/29/80";

#include "h01errs.h"

double
SQRT(value)

	double	value;
{
	if (value <= 0) {
		ERROR(ESQRT, value);
		return;
	}
	return sqrt(value);
}
