/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RSNG4.c 1.2 %G%";

#include "h01errs.h"

long
RSNG4(value, upper)

	long	value;
	long	upper;
{
	if (value < 0 || value > upper) {
		ERROR(ERANGE, value);
		return;
	}
	return	value;
}
