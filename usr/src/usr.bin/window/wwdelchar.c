#ifndef lint
static	char *sccsid = "@(#)wwdelchar.c	3.6 83/09/15";
#endif

#include "ww.h"
#include "tt.h"

wwdelchar(w, row, col)
register struct ww *w;
{
	register i;
	int nvis;

	/*
	 * First, shift the line.
	 */
	{
		register union ww_char *p, *q;

		p = &w->ww_buf[row][col];
		q = p + 1;
		for (i = w->ww_b.r - col; --i > 0;)
			*p++ = *q++;
		p->c_w = ' ';
	}

	/*
	 * If can't see it, just return.
	 */
	if (row < w->ww_i.t || row >= w->ww_i.b
	    || w->ww_i.r <= 0 || w->ww_i.r <= col)
		return;

	if (col < w->ww_i.l)
		col = w->ww_i.l;

	/*
	 * Now find out how much is actually changed, and fix wwns.
	 */
	{
		register union ww_char *buf;
		register char *win;
		register union ww_char *ns;
		register char *smap;
		char *touched;

		nvis = 0;
		smap = &wwsmap[row][col];
		for (i = w->ww_i.r - col; i > 0 && *smap++ != w->ww_index; i--)
			col++;
		if (i <= 0)
			return;
		buf = &w->ww_buf[row][col];
		win = &w->ww_win[row][col];
		ns = &wwns[row][col];
		touched = &wwtouched[row];
		for (; --i >= 0;) {
			if (*win) {
				if ((*win & (WWM_COV|WWM_GLS)) != 0) {
					ns++;
					buf++;
				} else {
					ns++->c_w = buf++->c_w
						^ *win++ << WWC_MSHIFT;
					*touched = 1;
				}
			} else {
				*touched = 1;
				*ns++ = *buf++;
				win++;
				nvis++;
			}
		}
	}

	/*
	 * Can/Should we use delete character?
	 */
	if (tt.tt_delchar != 0 && nvis > (wwncol - col) / 2) {
		register union ww_char *p, *q;

		(*tt.tt_move)(row, col);
		(*tt.tt_delchar)();

		p = &wwos[row][col];
		q = p + 1;
		for (i = wwncol - col; --i > 0;)
			*p++ = *q++;
		p->c_w = ' ';
	}
}
