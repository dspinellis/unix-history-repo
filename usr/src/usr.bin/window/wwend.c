#ifndef lint
static char sccsid[] = "@(#)wwend.c	3.7 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"
#include "tt.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
