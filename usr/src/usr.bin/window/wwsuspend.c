#ifndef lint
static	char *sccsid = "@(#)wwsuspend.c	3.3 83/11/02";
#endif

#include "ww.h"
#include "tt.h"
#include <signal.h>

#define mask(s)	(1<<(SIG/**/s-1))
wwsuspend()
{
	int (*oldsig)();

	oldmask = sigblock(mask(TSTP));
	wwend();
	sigsetmask(sigblock(0) & ~mask(TSTP));
	kill(0, SIGTSTP);
	sigblock(mask(TSTP));
	wwsettty(0, &wwnewtty);
	Winit(2, 1);
	sigsetmask(oldmask);
}
