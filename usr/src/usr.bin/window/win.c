#ifndef lint
static	char *sccsid = "@(#)win.c	3.2 84/01/16";
#endif

#include "defs.h"

/*
 * Routines for opening and closing windows
 */

/*
 * Open a user window.
 */
struct ww *
openwin(id, row, col, nrow, ncol, nline, label)
char *label;
{
	register struct ww *w;

	if (id < 0 && (id = findid()) < 0)
		return 0;
	if (row + nrow <= 0 || row > wwnrow - 1
	    || col + ncol <= 0 || col > wwncol - 1) {
		error("Illegal window position.");
		return 0;
	}
	if ((w = wwopen(WWO_PTY, nrow, ncol, row, col, nline)) == 0) {
		error("%s.", wwerror());
		return 0;
	}
	w->ww_id = id;
	window[id] = w;
	w->ww_hasframe = 1;
	w->ww_altpos.r = 1;
	w->ww_altpos.c = 0;
	if (label != 0 && setlabel(w, label) < 0)
		error("No memory for label.");
	wwcursor(w, 1);
	wwadd(w, framewin);
	selwin = w;
	reframe();
	wwupdate();
	wwflush();
	if (wwspawn(w, shell, shellname, (char *)0) < 0) {
		c_close(w);
		error("%s: %s.", shell, wwerror());
		return 0;
	}
	return w;
}

findid()
{
	register i;

	for (i = 0; i < NWINDOW && window[i] != 0; i++)
		;
	if (i >= NWINDOW) {
		error("Too many windows.");
		return -1;
	}
	return i;
}

/*
 * Close a user window.
 */
closewin(w)
register struct ww *w;
{
	if (w == selwin)
		selwin = 0;
	if (w == lastselwin)
		lastselwin = 0;
	if (w->ww_id >= 0 && w->ww_id < NWINDOW)
		window[w->ww_id] = 0;
	if (w->ww_label)
		str_free(w->ww_label);
	wwdelete(w);
	wwclose(w);
}

/*
 * Open an information (display) window.
 */
struct ww *
openiwin(nrow, label)
char *label;
{
	register struct ww *w;

	if ((w = wwopen(0, nrow, wwncol, 2, 0, 0)) == 0)
		return 0;
	w->ww_mapnl = 1;
	w->ww_hasframe = 1;
	w->ww_id = -1;
	w->ww_center = 1;
	(void) setlabel(w, label);
	wwadd(w, framewin);
	reframe();
	return w;
}

/*
 * Close an information window.
 */
closeiwin(w)
struct ww *w;
{
	closewin(w);
	reframe();
}

waitnl(w)
struct ww *w;
{
	(void) waitnl1(w, "[Type any key to continue]");
}

more(w, alway)
register struct ww *w;
char alway;
{
	int c;

	if (!alway && w->ww_cur.r < w->ww_w.b - 2)
		return 0;
	c = waitnl1(w, "[Type escape to abort, any other key to continue]");
	(void) wwputs("\033E", w);
	return c == CTRL([) ? 2 : 1;
}

waitnl1(w, prompt)
register struct ww *w;
char *prompt;
{
	front(w, 0);
	(void) wwprintf(w, "\033Y%c%c\033p%s\033q ",
		w->ww_w.nr - 1 + ' ', ' ', prompt);	/* print on last line */
	wwcurtowin(w);
	while (wwpeekc() < 0)
		wwiomux();
	return wwgetc();
}
