#ifndef lint
static	char *sccsid = "@(#)wwsuspend.c	3.2 83/08/15";
#endif

#include "ww.h"
#include "tt.h"
#include <signal.h>

#define mask(s)	(1 << (s) - 1)

wwsuspend()
{
	int oldmask;

	oldmask = sigblock(mask(SIGTSTP));
	wwend();
	(void) sigsetmask(sigblock(0) & ~mask(SIGTSTP));
	(void) kill(0, SIGTSTP);
	(void) sigblock(mask(SIGTSTP));
	(void) wwsettty(0, &wwnewtty);
	(*tt.tt_init)();
	wwredraw();
	(void) sigsetmask(oldmask);
}
