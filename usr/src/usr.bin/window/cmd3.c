#ifndef lint
static	char *sccsid = "@(#)cmd3.c	1.4 83/07/28";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

doclose(flag, w)
register struct ww *w;
{
	char didit = 0;

	switch (flag) {
	case CLOSE_ONE:
		if (w == 0)
			break;
		if (w == selwin)
			setselwin(0);
		wwclose(w);
		didit++;
		break;
	case CLOSE_DEAD:
	case CLOSE_ALL:
		for (w = wwhead; w;) {
			if (w != cmdwin
			    && (w->ww_state == WW_DEAD || flag == CLOSE_ALL)) {
				struct ww *w1;
				w = (w1 = w)->ww_next;
				if (w1 == selwin)
					setselwin(0);
				if (w->ww_state == WW_HASPROC && w->ww_pid == 0)
				{
					wwprintf(cmdwin, "%d: pid == 0.  ",
						w->ww_ident);
				} else {
					wwclose(w1);
					didit++;
				}
			} else
				w = w->ww_next;
		}
		break;
	}
	if (selwin == 0) {
		for (w = wwhead; w && w == cmdwin; w = w->ww_next)
			;
		setselwin(w);
	}
	if (didit)
		reframe();
}

doescape()
{
	char buf[2];

	wwputs("New escape character? ", cmdwin);
	wwsetcursor(WCurRow(cmdwin->ww_win), WCurCol(cmdwin->ww_win));
	while ((*buf = bgetc()) < 0)
		bread();
	buf[1] = 0;
	setescape(buf);
	wwputs("\r\n", cmdwin);
}

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
