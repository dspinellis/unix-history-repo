/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SQRT.c 1.4 %G%";

#include <math.h>

double
SQRT(value)

	double	value;
{
	if (value < 0) {
		ERROR("Negative argument of %e to sqrt\n", value);
	}
	return sqrt(value);
}
