/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)kon.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Turn on keypad, so it sends codes instead of doing them in local.
 */

#include "2648.h"

kon()
{
	escseq(NONE);
	outstr("\33&s1A");
}

koff()
{
	escseq(NONE);
	outstr("\33&s0A");
}
