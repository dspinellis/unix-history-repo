/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)CHR.c 1.1 %G%";

#include "h01errs.h"

CHR(value)

	long	value;
{
	if (value < 0 || value > 127) {
		ERROR(ECHR, value);
		return;
	}
	return value;
}
