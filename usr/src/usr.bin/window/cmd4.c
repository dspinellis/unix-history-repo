#ifndef lint
static	char *sccsid = "@(#)cmd4.c	3.9 84/04/05";
#endif

#include "defs.h"

c_show()
{
	register i;
	register struct ww *w = 0;
	char done_it = 0;

	for (i = 0; i < NWINDOW; i++) {
		if ((w = window[i]) == 0)
			continue;
		done_it++;
		if (!terse && cmdwin->ww_order < framewin->ww_order) {
			wwdelete(cmdwin);
			wwadd(cmdwin, framewin);
		}
		front(w, 0);
		wwsetcursor(w->ww_w.t - 1, w->ww_w.l + 1);
		for (;;) {
			switch (wwgetc()) {
			case '\r':
			case '\n':
				break;
			case CTRL([):
				setselwin(w);
				goto out;
			case -1:
				wwiomux();
				continue;
			default:
				wwputc(CTRL(g), cmdwin);
				if (!terse) {
					wwputs("\rType return to continue, escape to select.", cmdwin);
					wwdelete(cmdwin);
					wwadd(cmdwin, &wwhead);
				}
				continue;
			}
			break;
		}
	}
out:
	if (!done_it) {
		error("No windows.");
	} else {
		if (!terse) {
			wwdelete(cmdwin);
			wwadd(cmdwin, &wwhead);
			wwputs("\r\n", cmdwin);
		}
	}
}

c_colon()
{
	char oldterse = terse;
	char buf[512];

	setterse(0);
	wwputc(':', cmdwin);
	wwgets(buf, wwncol - 3, cmdwin);
	wwputs("\r\n", cmdwin);
	wwcurtowin(cmdwin);
	setterse(oldterse);
	if (dolongcmd(buf) < 0)
		error("Out of memory.");
}
