#ifndef lint
static char sccsid[] = "@(#)close.c	4.1 (Berkeley) %G%";
#endif

#include <signal.h>
#include "bg.h"

closepl()
{
	/* recieve interupts */
	signal(SIGINT, SIG_IGN);

	/* exit graphics mode */
	putchar( ESC );
	printf("[H");
	exit(0);
}
