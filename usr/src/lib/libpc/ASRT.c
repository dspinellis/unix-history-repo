/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ASRT.c 1.1 10/29/80";

#include "h01errs.h"

ASRT(cond, stmt)

	short	cond;
	char	*stmt;
{
	if (cond)
		return;
	ERROR(EASRT, stmt);
}
