/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)LN.c 1.1 10/29/80";

#include "h01errs.h"

double
LN(value)

	double	value;
{
	if (value <= 0) {
		ERROR(ELN, value);
		return;
	}
	return log(value);
}
