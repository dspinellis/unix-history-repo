#ifndef lint
static	char *sccsid = "@(#)cmd3.c	3.3 83/08/22";
#endif

#include "defs.h"

c_close(w)
register struct ww *w;
{
	char didit = 0;
	register i;

	if (w != 0) {
		closewin(w);
		didit++;
	} else {
		for (i = 0; i < NWINDOW; i++) {
			if ((w = window[i]) == 0)
				continue;
			closewin(w);
			didit++;
		}
	}
	if (selwin == 0) {
		for (i = 0; i < NWINDOW && window[i] == 0; i++)
			;
		if (i < NWINDOW)
			setselwin(window[i]);
	}
	if (didit)
		reframe();
}

closewin(w)
register struct ww *w;
{
	if (w == selwin)
		setselwin((struct ww *)0);
	if (w->ww_id >= 0 && w->ww_id < NWINDOW)
		window[w->ww_id] = 0;
	if (w->ww_label)
		free(w->ww_label);
	wwdelete(w);
	wwclose(w);
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

setlabel(w, label)
register struct ww *w;
char *label;
{
	char *malloc();

	if (w->ww_label != 0)
		free(w->ww_label);
	if ((w->ww_label = malloc((unsigned)strlen(label) + 1)) == 0)
		return -1;
	(void) strcpy(w->ww_label, label);
	return 0;
}
