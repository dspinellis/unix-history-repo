#ifndef lint
static	char *sccsid = "@(#)cmd4.c	3.3 83/08/22";
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
		wwdelete(w);
		wwadd(w, framewin);
		reframe();
		wwsetcursor(w->ww_w.t - 1, w->ww_w.l + 1);
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
				wwbell();
				if (!terse) {
					(void) wwputs("\rType return to continue, escape to select.", cmdwin);
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
			(void) wwputs("\r\n", cmdwin);
		}
	}
}

c_colon()
{
	char buf[512];

	if (terse)
		wwadd(cmdwin, &wwhead);
	(void) wwputc(':', cmdwin);
	bgets(buf, wwncol - 3, cmdwin);
	(void) wwputs("\r\n", cmdwin);
	if (terse)
		wwdelete(cmdwin);
	else
		wwcurtowin(cmdwin);
	dolongcmd(buf);
}
