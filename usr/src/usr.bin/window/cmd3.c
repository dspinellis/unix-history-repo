#ifndef lint
static	char *sccsid = "@(#)cmd3.c	1.2 83/07/19";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

doclose(c)
{
	register struct ww *w;

	switch (c) {
	case 'c':
		if ((w = getwin()) == 0)
			break;
		if (w == selwin)
			setselwin(0);
		wwclose(w);
		break;
	case 'C':
	case 'Z':
		for (w = wwhead; w;) {
			if (w != cmdwin
			    && (w->ww_state == WW_DEAD || c == 'Z')) {
				struct ww *w1;
				w = (w1 = w)->ww_next;
				if (w1 == selwin)
					setselwin(0);
				if (w->ww_state == WW_HASPROC && w->ww_pid == 0)
				{
					wwprintf(cmdwin, "%d: pid == 0.  ",
						w->ww_ident);
				} else
					wwclose(w1);
			} else
				w = w->ww_next;
		}
		break;
	}
	if (selwin == 0) {
		for (w = wwhead; w && w == cmdwin;
		     w = w->ww_next)
			;
		setselwin(w);
	}
}
