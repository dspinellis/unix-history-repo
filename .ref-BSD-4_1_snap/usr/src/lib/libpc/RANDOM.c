/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RANDOM.c 1.3 3/7/81";

#include "h00vars.h"

extern long RAND();

double
RANDOM()
{
	/*
	 * div by maxint to get 0..1
	 */
	_seed = RAND(_seed);
	return((double)_seed / 0x7fffffff);
}
