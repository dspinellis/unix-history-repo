#ifndef lint
static char sccsid[] = "@(#)wwsuspend.c	3.7 4/24/85";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"
#include "tt.h"
#include <sys/signal.h>

wwsuspend()
{
	int (*oldsig)();

	oldsig = signal(SIGTSTP, SIG_IGN);
	wwend();
	(void) signal(SIGTSTP, SIG_DFL);
	(void) kill(0, SIGTSTP);
	(void) signal(SIGTSTP, SIG_IGN);
	(void) wwsettty(0, &wwnewtty, &wwoldtty);
	(*tt.tt_init)();
	wwredraw();
	(void) signal(SIGTSTP, oldsig);
}
