/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RANDOM.c 1.4 1/10/83";

#include "h00vars.h"

extern long RAND();

double
RANDOM()
{
	double d;
	long l;

	/*
	 * calculate (1103515245 * seed) mod 2^31-1
	 */
	d = 1103515245.0 * _seed / 2147483647.0;
	l = d;
	d = d - l;
	_seed = d * 2147483647.0;
	/*
	 * want a value in the range 0..1
	 */
	return(d);
}
