/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cmd3.c	3.18 (Berkeley) %G%";
#endif /* not lint */

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
