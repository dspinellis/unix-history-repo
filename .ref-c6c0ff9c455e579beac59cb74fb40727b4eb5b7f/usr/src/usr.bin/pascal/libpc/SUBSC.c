/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SUBSC.c 1.1 %G%";

#include	"h01errs.h"

SUBSC(i, lower, upper)

	long	i, lower, upper;
{
	if (i < lower || i > upper) {
		ERROR(ESUBSC, i);
	}
	return i;
}
