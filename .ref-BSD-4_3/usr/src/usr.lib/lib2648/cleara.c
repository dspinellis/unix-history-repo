/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cleara.c	5.1 (Berkeley) 4/26/85";
#endif not lint

#include "2648.h"

cleara()
{
	sync();
	escseq(NONE);
	outstr("\33H\33J");
}
