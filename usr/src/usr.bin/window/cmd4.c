#ifndef lint
static	char *sccsid = "@(#)cmd4.c	2.1 83/07/30";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

doshow()
{
	register i;
	register struct ww *w = 0;
	char done_it = 0;

	for (i = 1; i < 10; i++) {
		if ((w = wwfind(i)) == 0)
			continue;
		done_it++;
		wwsetcurwin(w);
		wwsetcursor(w->ww_o.row, w->ww_o.col + 1);
		for (;;) {
			switch (bgetc()) {
			case '\r':
			case '\n':
				break;
			case CTRL([):
				setselwin(w);
				goto out;
			case -1:
				bread();
				continue;
			default:
				if (terse)
					Ding();
				else
					wwputs("\rType return to continue, escape to select.", cmdwin);
				wwsetcurwin(cmdwin);
				Ding();
				continue;
			}
			break;
		}
	}
out:
	if (!done_it) {
		if (terse)
			Ding();
		else
			wwputs("No windows.  ", cmdwin);
	} else {
		wwsetcurwin(cmdwin);
		if (!terse)
			wwputs("\r\n", cmdwin);
	}
}

docolon()
{
	char buf[512];

	if (terse)
		Wunhide(cmdwin->ww_win);
	wwputc(':', cmdwin);
	bgets(buf, wwncol - 3, cmdwin);
	wwputs("\r\n", cmdwin);
	if (terse)
		Whide(cmdwin->ww_win);
	dolongcmd(buf);
}
