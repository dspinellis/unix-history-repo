#ifndef lint
static	char *sccsid = "@(#)cmd3.c	1.1 83/07/18";
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
