/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)STLIM.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

STLIM(limit)

	long	limit;
{
	if (_stcnt >= limit) {
		ERROR(ESTLIM, _stcnt);
		return;
	}
	_stlim = limit;
}
