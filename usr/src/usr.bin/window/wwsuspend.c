#ifndef lint
static	char *sccsid = "@(#)wwsuspend.c	1.1 83/07/17";
#endif

#include "ww.h"
#include <signal.h>

#define mask(s)	(1<<(SIG/**/s-1))

wwsuspend()
{
	int oldmask;

	oldmask = sigblock(mask(TSTP));
	wwend();
	sigsetmask(sigblock(0) & ~mask(TSTP));
	kill(0, SIGTSTP);
	sigblock(mask(TSTP));
	wwsettty(0, &wwnewtty);
	Winit(2, 1);
	sigsetmask(oldmask);
}
