#ifndef lint
static	char *sccsid = "@(#)cmd4.c	1.1 83/07/19";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

doquery()
{
	register i;
	register struct ww *w = 0;
	char done_it = 0;

	for (i = 1; i < 10; i++) {
		if ((w = wwfind(i)) == 0)
			continue;
		done_it++;
		wwsetcurrent(w);
		wwsetcursor(w->ww_row, w->ww_col + 1);
		for (;;) {
			switch (bgetc()) {
			case '\r':
			case '\n':
				break;
			case CTRL([):
				setselwin(w);
				return;
			case -1:
				bread();
				continue;
			default:
				wwputs("\rType return to continue, escape to select.", cmdwin);
				wwsetcurrent(cmdwin);
				Ding();
				continue;
			}
			break;
		}
	}
	if (!done_it)
		wwputs("No windows.  ", cmdwin);
	else {
		wwsetcurrent(cmdwin);
		wwputs("\r\n", cmdwin);
	}
}
