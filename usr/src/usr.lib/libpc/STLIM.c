/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)STLIM.c 1.2 6/10/81";

#include "h00vars.h"

STLIM(limit)

	long	limit;
{
	if (_stcnt >= limit) {
		ERROR("Statement count limit of %D exceeded\n", _stcnt);
		return;
	}
	_stlim = limit;
}
