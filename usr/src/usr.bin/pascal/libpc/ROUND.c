/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ROUND.c 1.2 %G%";

ROUND(value)

	double	value;
{
	if (value >= 0.0)
		return (long)(value + 0.5);
	return (long)(value - 0.5);
}
