#ifndef lint
static	char *sccsid = "@(#)wwsuspend.c	3.4 84/04/16";
#endif

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
	(void) wwsettty(0, &wwnewtty);
	(*tt.tt_init)();
	wwredraw();
	(void) signal(SIGTSTP, oldsig);
}
