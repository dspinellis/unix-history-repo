/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RANG4.c 1.3 6/10/81";


long
RANG4(value, lower, upper)

	long	value;
	long	lower;
	long	upper;
{
	if (value < lower || value > upper) {
		ERROR("Value of %D is out of range\n", value);
		return;
	}
	return	value;
}
