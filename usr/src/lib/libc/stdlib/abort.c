/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)abort.c	5.4 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

/* C library -- abort */

#include "signal.h"

abort()
{
	sigblock(~0L);
	signal(SIGILL, SIG_DFL);
	sigsetmask(~sigmask(SIGILL));
	kill(getpid(), SIGILL);
}
