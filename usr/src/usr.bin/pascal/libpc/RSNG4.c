/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RSNG4.c 1.1 %G%";


#include "h01errs.h"

RSNG4(value, upper)

	int	value;
	int	upper;
{
	if (value < 0 || value > upper) {
		ERROR(ERANGE, value);
		return;
	}
	return	value;
}
