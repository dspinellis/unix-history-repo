#ifndef lint
static char sccsid[] = "@(#)wwsuspend.c	3.6 %G%";
#endif

#include "ww.h"
#include "tt.h"
#include <sys/signal.h>

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
