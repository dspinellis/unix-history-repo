/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RSNG4.c 1.3 6/10/81";


long
RSNG4(value, upper)

	long	value;
	long	upper;
{
	if (value < 0 || value > upper) {
		ERROR("Value of %D is out of range\n", value);
		return;
	}
	return	value;
}
