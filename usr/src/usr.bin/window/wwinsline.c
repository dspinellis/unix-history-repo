#ifndef lint
static	char *sccsid = "@(#)wwinsline.c	3.4 83/09/14";
#endif

#include "ww.h"

wwinsline(w, line)
register struct ww *w;
int line;
{
	register i;
	register union ww_char **cpp, **cqq;
	register union ww_char *cp;
	int row11, row2;
	char deleted;
	int visible;

	/*
	 * Scroll first.
	 */
	if ((row11 = line - w->ww_scroll) < w->ww_i.t - w->ww_w.t) {
		row11 = 0;
		visible = 0;
	} else
		visible = 1;
	if ((row2 = w->ww_nline - w->ww_scroll) > w->ww_i.b - w->ww_w.t)
		row2 = w->ww_i.b - w->ww_w.t;
	deleted = wwscroll1(w, row11, row2, -1, visible);

	/*
	 * Fix the buffer.
	 * But leave clearing the last line for wwclreol().
	 */
	cpp = &w->ww_buf[w->ww_nline];
	cqq = cpp - 1;
	cp = *cqq;
	for (i = w->ww_nline - line - 1; --i >= 0;)
		*--cpp = *--cqq;
	*cqq = cp;

	/*
	 * Now clear the last line.
	 */
	if (visible)
		wwclreol1(w, line, 0, deleted);
	else
		for (i = w->ww_w.nc; --i >= 0;)
			cp++->c_w = ' ';
}
