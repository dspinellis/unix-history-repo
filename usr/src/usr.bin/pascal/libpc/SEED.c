/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SEED.c 1.4 %G%";

#include "h00vars.h"

SEED(value)
	long value;
{
	long tmp;

	tmp = _seed;
	_seed = value;
	return tmp;
}
