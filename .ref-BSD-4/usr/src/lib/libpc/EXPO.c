/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)EXPO.c 1.1 10/29/80";

EXPO(value)

	long	value;
{
	if (value == 0)
		return 0;
	return ((value & ~0xffff8000) >> 7) - 128;
}
