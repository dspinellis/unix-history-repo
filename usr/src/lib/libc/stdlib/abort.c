/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)abort.c	5.1 (Berkeley) %G%";
#endif not lint

/* C library -- abort */

#include "signal.h"

abort()
{
	sigblock(~0);
	signal(SIGILL, SIG_DFL);
	sigsetmask(~sigmask(SIGILL));
	kill(0, SIGILL);
}
