#ifndef lint
static	char *sccsid = "@(#)wwdelchar.c	3.5 83/09/14";
#endif

#include "ww.h"
#include "tt.h"

wwdelchar(w, line, col)
register struct ww *w;
{
	register i;
	int row = line - w->ww_scroll;
	int srow = row + w->ww_w.t;
	int scol = col + w->ww_w.l;
	int nvis;

	/*
	 * First, shift the line.
	 */
	{
		register union ww_char *p, *q;

		p = &w->ww_buf[line][col];
		q = p + 1;
		for (i = w->ww_w.nc - col - 1; --i >= 0;)
			*p++ = *q++;
		p->c_w = ' ';
	}

	/*
	 * If can't see it, just return.
	 */
	if (srow < w->ww_i.t || srow >= w->ww_i.b
	    || w->ww_i.r <= 0 || w->ww_i.r <= scol)
		return;

	if (scol < w->ww_i.l)
		scol = w->ww_i.l;
	col = scol - w->ww_w.l;

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
		smap = &wwsmap[srow][scol];
		for (i = w->ww_i.r - scol; i > 0 && *smap++ != w->ww_index; i--)
			col++, scol++;
		if (i <= 0)
			return;
		buf = &w->ww_buf[line][col];
		win = &w->ww_win[row][col];
		ns = &wwns[srow][scol];
		touched = &wwtouched[srow];
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
	if (tt.tt_delchar != 0 && nvis > (wwncol - scol) / 2) {
		register union ww_char *p, *q;

		(*tt.tt_move)(srow, scol);
		(*tt.tt_delchar)();

		p = &wwos[srow][scol];
		q = p + 1;
		for (i = wwncol - scol; --i > 0;)
			*p++ = *q++;
		p->c_w = ' ';
	}
}
