/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SCLCK.c 1.2 %G%";

long
SCLCK()
{
	long	tim[4];

	times(tim);
	return (tim[1] * 50) / 3;
}
