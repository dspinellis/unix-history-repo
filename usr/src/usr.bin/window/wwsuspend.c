/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwsuspend.c	3.12 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"
#include <sys/signal.h>

wwsuspend()
{
	sig_t oldsig;

	oldsig = signal(SIGTSTP, SIG_IGN);
	wwend();
	(void) signal(SIGTSTP, SIG_DFL);
	(void) kill(0, SIGTSTP);
	(void) signal(SIGTSTP, SIG_IGN);
	(void) wwsettty(0, &wwnewtty, &wwoldtty);
	xxstart();
	wwredraw();		/* XXX, clears the screen twice */
	(void) signal(SIGTSTP, oldsig);
}
