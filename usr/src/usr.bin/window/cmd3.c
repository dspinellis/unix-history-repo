#ifndef lint
static char sccsid[] = "@(#)cmd3.c	3.14 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "defs.h"
#include "string.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

doclose(w)
setescape(esc)
register char *esc;
{
	if (*esc == '^') {
		if (esc[1] != 0)
			escapec = esc[1] & 0x1f;
		else
			escapec = '^';
	} else
		escapec = *esc;
}

/*
dolabel()
{
	register struct ww *w;
	char buf[30];
	char *malloc();

	if ((w = getwin()) == 0)
		return;
	wwprintf(cmdwin, "Label for window %d? ", w->ww_ident);
	bgets(buf, sizeof buf, cmdwin);
	setlabel(w, buf);
	wwputs("\r\n", cmdwin);
}
*/

setlabel(w, label)
register struct ww *w;
char *label;
{
	if (w->ww_label != 0)
}
