/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)CLCK.c 1.1 10/29/80";

CLCK()
{
	long	tim[4];

	times(tim);
	return (tim[0] * 50) / 3;
}
