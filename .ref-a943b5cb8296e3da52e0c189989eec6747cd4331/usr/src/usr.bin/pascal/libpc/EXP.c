/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)EXP.c 1.1 %G%";

#include <math.h>
extern int errno;

double
EXP(value)
	double	value;
{
	double result;

	errno = 0;
	result = exp(value);
	if (errno != 0) {
		ERROR("exp(%e) yields a result that is out of the range of reals\n", value);
		return;
	}
	return result;
}
