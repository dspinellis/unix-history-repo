/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SEED.c 1.5 3/7/81";

#include "h00vars.h"

long
SEED(value)
	long value;
{
	long tmp;

	tmp = _seed;
	_seed = value;
	return tmp;
}
