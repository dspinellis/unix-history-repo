/*	abort.c	4.1	85/01/24	*/

/* C library -- abort */

#include "signal.h"

abort()
{
	sigblock(~0);
	signal(SIGILL, SIG_DFL);
	sigsetmask(~sigmask(SIGILL));
	kill(0, SIGILL);
}
