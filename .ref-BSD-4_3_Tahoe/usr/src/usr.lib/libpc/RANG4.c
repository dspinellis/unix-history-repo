/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RANG4.c 1.4 11/12/82";

char ERANG[] = "Value of %D is out of range\n";

long
RANG4(value, lower, upper)
	long	value;
	long	lower;
	long	upper;
{
	if (value < lower || value > upper) {
		ERROR(ERANG, value);
		return;
	}
	return	value;
}
