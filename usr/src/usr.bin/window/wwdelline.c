#ifndef lint
static	char *sccsid = "@(#)wwdelline.c	3.2 83/08/11";
#endif

#include "ww.h"

wwdelline(w, line)
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
	deleted = wwscroll1(w, srow, erow, 1, 1);

	/*
	 * Fix the buffer.
	 * But leave clearing the last line for wwclreol().
	 */
	cpp = &w->ww_buf[line];
	cqq = cpp + 1;
	tmp = *cpp;
	for (i = w->ww_nline - line - 1; --i >= 0;)
		*cpp++ = *cqq++;
	*cpp = tmp;

	/*
	 * Now clear the last line.
	 */
	wwclreol1(w, w->ww_nline - 1, 0, deleted);
}
