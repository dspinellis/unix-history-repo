/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RANDOM.c 1.1 %G%";

double
RANDOM()
{
	/*
	 * div by maxint to get 0..1
	 */
	return (rand() / 2.147483647e+09);
}
