#ifndef lint
static char sccsid[] = "@(#)wwflush.c	3.7 4/24/85";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"
#include "tt.h"

wwflush()
{
	if (wwcursorrow < 0 || wwcursorrow >= wwnrow
	    || wwcursorcol < 0 || wwcursorcol >= wwncol)
		(*tt.tt_move)(0, 0);
	else
		(*tt.tt_move)(wwcursorrow, wwcursorcol);
	ttflush();
}
