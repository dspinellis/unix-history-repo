/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)LINO.c 1.2 6/10/81";

#include "h00vars.h"

LINO()
{
	if (++_stcnt >= _stlim) {
		ERROR("Statement count limit of %D exceeded\n", _stcnt);
		return;
	}
}
