/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)HALT.c 1.1 %G%";

#include "h01errs.h"

HALT()
{
	ERROR(EHALT, 0);
}
