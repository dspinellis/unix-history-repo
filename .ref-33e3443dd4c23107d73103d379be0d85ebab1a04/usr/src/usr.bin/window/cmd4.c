#ifndef lint
static char sccsid[] = "@(#)cmd4.c	3.14 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

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
	if (dolongcmd(buf, (struct value *)0, 0) < 0)
		error("Out of memory.");
}
