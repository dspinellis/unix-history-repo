/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SUBSC.c 1.5 %G%";

char ESUBSC[] = "Subscript value of %D is out of range\n";

long
SUBSC(i, lower, upper)

	long	i, lower, upper;
{
	if (i < lower || i > upper) {
		ERROR(ESUBSC, i);
	}
	return i;
}
