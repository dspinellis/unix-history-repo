/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)dispmsg.c	5.1 (Berkeley) 4/26/85";
#endif not lint

/*
 * display a message, str, starting at (x, y).
 */

#include "2648.h"

dispmsg(str, x, y, maxlen)
char *str;
int x, y;
{
	int oldx, oldy;
	int oldcuron;
	int oldquiet;
	extern int QUIET;

	oldx = _curx; oldy = _cury;
	oldcuron = _cursoron;
	zoomout();
	areaclear(y, x, y+8, x+6*maxlen);
	setset();
	curon();
	movecurs(x, y);
	texton();
	oldquiet = QUIET;
	QUIET = 0;
	outstr(str);
	if (oldquiet)
		outstr("\r\n");
	QUIET = oldquiet;
	textoff();
	movecurs(oldx, oldy);
	if (oldcuron == 0)
		curoff();
}
