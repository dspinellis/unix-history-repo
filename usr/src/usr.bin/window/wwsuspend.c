#ifndef lint
static	char *sccsid = "@(#)wwsuspend.c	3.1 83/08/11";
#endif

#include "ww.h"
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
	(*tt.tt_reset)();
	wwredraw();
	(void) sigsetmask(oldmask);
}
