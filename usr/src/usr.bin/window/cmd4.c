#ifndef lint
static	char *sccsid = "@(#)cmd4.c	3.12 84/05/06";
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
	if (dolongcmd(buf, (struct value *)0, 0) < 0)
		error("Out of memory.");
}
