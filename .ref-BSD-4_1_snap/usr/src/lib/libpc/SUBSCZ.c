/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SUBSCZ.c 1.3 6/10/81";


long
SUBSCZ(i, upper)

	long	i, upper;
{
	if (i < 0 || i > upper) {
		ERROR("Subscript value of %D is out of range\n", i);
		return;
	}
	return i;
}
