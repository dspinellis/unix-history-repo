#ifndef lint
static char sccsid[] = "@(#)cmd3.c	3.13 %G%";
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
register struct ww *w;
{
	char didit = 0;
	struct ww *w1;

	if (w != 0) {
		if (w == selwin)
			setselwin(0);
		wwclose(w);
		didit++;
	} else {
		for (w = wwhead; w;) {
			if (w == cmdwin) {
				w = w->ww_next;
				continue;
			}
			w = (w1 = w)->ww_next;
			if (w1 == selwin)
				setselwin(0);
			if (w->ww_state == WW_HASPROC && w->ww_pid == 0) {
				wwprintf(cmdwin, "%d: pid == 0.  ",
					w->ww_ident);
			} else {
				wwclose(w1);
				didit++;
			}
		}
	}
	if (selwin == 0) {
		for (w = wwhead; w && w == cmdwin; w = w->ww_next)
	}
	if (didit)
		reframe();
}
*/

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
