/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SUBSC.c 1.3 6/10/81";


long
SUBSC(i, lower, upper)

	long	i, lower, upper;
{
	if (i < lower || i > upper) {
		ERROR("Subscript value of %D is out of range\n", i);
		return;
	}
	return i;
}
