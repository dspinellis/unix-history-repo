#ifndef lint
static	char *sccsid = "@(#)wwdelline.c	3.4 83/09/14";
#endif

#include "ww.h"

wwdelline(w, line)
register struct ww *w;
int line;
{
	register i;
	register union ww_char **cpp, **cqq;
	register union ww_char *cp;
	int row1, row2;
	char deleted;
	int visible;

	/*
	 * Scroll first.
	 */
	if ((row1 = line - w->ww_scroll) < w->ww_i.t - w->ww_w.t)
		row1 = w->ww_i.t - w->ww_w.t;
	if ((row2 = w->ww_nline - w->ww_scroll) > w->ww_i.b - w->ww_w.t) {
		row2 = w->ww_i.b - w->ww_w.t;
		visible = 0;
	} else
		visible = 1;
	deleted = wwscroll1(w, row1, row2, 1, visible);

	/*
	 * Fix the buffer.
	 * But leave clearing the last line for wwclreol().
	 */
	cpp = &w->ww_buf[line];
	cqq = cpp + 1;
	cp = *cpp;
	for (i = w->ww_nline - line; --i > 0;)
		*cpp++ = *cqq++;
	*cpp = cp;

	/*
	 * Now clear the last line.
	 */
	if (visible)
		wwclreol1(w, w->ww_nline - 1, 0, deleted);
	else
		for (i = w->ww_w.nc; --i >= 0;)
			cp++->c_w = ' ';
}
