/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SUBSCZ.c 1.1 10/29/80";

#include	"h01errs.h"

SUBSCZ(i, upper)

	long	i, upper;
{
	if (i < 0 || i > upper) {
		ERROR(ESUBSC, i);
	}
	return i;
}
