/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ROUND.c 1.1 10/29/80";

ROUND(value)

	double	value;
{
	return (long)(value + 0.5);
}
