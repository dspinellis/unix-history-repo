#ifndef lint
static	char *sccsid = "@(#)cmd4.c	3.11 84/04/08";
#endif

#include "defs.h"

c_colon()
{
	char oldterse = terse;
	char buf[512];

	setterse(0);
	wwputc(':', cmdwin);
	wwgets(buf, wwncol - 3, cmdwin);
	wwputc('\n', cmdwin);
	wwcurtowin(cmdwin);
	setterse(oldterse);
	if (dolongcmd(buf) < 0)
		error("Out of memory.");
}
