/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PRED.c 1.5 %G%";


long
PRED(value, lower, upper)

	long	value;
	long	lower;
	long	upper;
{
	if (value == lower) {
		ERROR("Cannot take pred of first element of a range\n");
	}
	value--;
	if (value < lower || value > upper) {
		ERROR("Value of %D is out of range\n", value);
	}
	return	value;
}
