/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwsuspend.c	3.15 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"
#include <sys/signal.h>

void
#define mask(s)	(1<<(SIG/**/s-1))
wwsuspend()
{
	sig_t oldsig;

	oldmask = sigblock(mask(TSTP));
	wwend();
	sigsetmask(sigblock(0) & ~mask(TSTP));
	kill(0, SIGTSTP);
	sigblock(mask(TSTP));
	wwsettty(0, &wwnewtty);
	Winit(2, 1);
	sigsetmask(oldmask);
}
