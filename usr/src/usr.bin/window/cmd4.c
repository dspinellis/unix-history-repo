#ifndef lint
static	char *sccsid = "@(#)cmd4.c	3.11 84/04/08";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

doshow()
docolon()
{
	char oldterse = terse;
	char buf[512];

	wwgets(buf, wwncol - 3, cmdwin);
	wwputs("\r\n", cmdwin);
	if (dolongcmd(buf) < 0)
		error("Out of memory.");
}
