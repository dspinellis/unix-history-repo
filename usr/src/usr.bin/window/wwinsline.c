#ifndef lint
static	char *sccsid = "@(#)wwinsline.c	3.2 83/08/11";
#endif

#include "ww.h"

wwinsline(w, line)
register struct ww *w;
int line;
{
	register i;
	register union ww_char **cpp, **cqq;
	union ww_char *tmp;
	int srow, erow;
	char deleted;

	/*
	 * Scroll first.
	 */
	if ((srow = line - w->ww_scroll) < 0)
		srow = 0;
	if ((erow = w->ww_nline - w->ww_scroll - 1) >= w->ww_w.nr)
		erow = w->ww_w.nr - 1;
	deleted = wwscroll1(w, srow, erow, -1, 1);

	/*
	 * Fix the buffer.
	 * But leave clearing the last line for wwclreol().
	 */
	cpp = &w->ww_buf[w->ww_nline];
	cqq = cpp - 1;
	tmp = *cqq;
	for (i = w->ww_nline - line - 1; --i >= 0;)
		*--cpp = *--cqq;
	*cqq = tmp;

	/*
	 * Now clear the last line.
	 */
	wwclreol1(w, line, 0, deleted);
}
