/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SEED.c 1.2 %G%";

#include <math.h>
#include <math.h>

SEED(value)

	long	value;
{
	static long	seed;
	long		tmp;

	srand(value);
	tmp = seed;
	seed = value;
	return tmp;
}
