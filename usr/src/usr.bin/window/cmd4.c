/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)cmd4.c	3.15 (Berkeley) %G%";
#endif /* not lint */

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
