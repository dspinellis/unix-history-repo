/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)COS.c 1.1 2/8/82";

#include <math.h>
extern int errno;

double
COS(value)
	double	value;
{
	double result;

	errno = 0;
	result = cos(value);
	if (errno != 0) {
		ERROR("Cannot compute cos(%e)\n", value);
		return;
	}
	return result;
}
