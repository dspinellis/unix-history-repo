/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)LINO.c 1.3 11/12/82";

#include "h00vars.h"

char ELINO[] = "Statement count limit of %D exceeded\n";

LINO()
{
	if (++_stcnt >= _stlim) {
		ERROR(ELINO, _stcnt);
		return;
	}
}
