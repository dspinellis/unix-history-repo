#ifndef lint
static	char *sccsid = "@(#)wwinschar.c	3.2 83/08/11";
#endif

#include "ww.h"

wwinschar(w, line, col, c)
register struct ww *w;
short c;
{
	register i;
	int row = line - w->ww_scroll;
	int nvis;

	{
		register union ww_char *p, *q;

		p = &w->ww_buf[line][w->ww_w.nc];
		q = p - 1;
		for (i = w->ww_w.nc - col - 1; --i >= 0;)
			*--p = *--q;
		q->c_w = c;
	}
	if (row < 0 || row >= w->ww_w.nr)
		return;
	{
		register union ww_char *buf;
		register char *win;
		register union ww_char *ns;
		register char *smap;

		nvis = 0;
		smap = &wwsmap[row + w->ww_w.t][col + w->ww_w.l];
		for (i = w->ww_w.nc - col; i > 0; i--)
			if (*smap == w->ww_index)
				break;
			else {
				smap++;
				col++;
			}
		if (i <= 0)
			return;
		buf = &w->ww_buf[line][col];
		win = &w->ww_win[row][col];
		ns = &wwns[row + w->ww_w.t][col + w->ww_w.l];
		c = buf->c_w ^ *win << WWC_MSHIFT;
		for (; --i >= 0;) {
			if (*win) {
				if ((*win & (WWM_COV|WWM_GLS)) != 0) {
					ns++;
					buf++;
				} else
					ns++->c_w = buf++->c_w
						^ *win++ << WWC_MSHIFT;
			} else {
				*ns++ = *buf++;
				win++;
				nvis++;
			}
		}
	}
	col += w->ww_w.l;
	row += w->ww_w.t;
	if (nvis > (wwncol - col) / 2) {
		register union ww_char *p, *q;

		(*tt.tt_setinsert)(1);
		(*tt.tt_move)(row, col);
		(*tt.tt_setmodes)(c >> WWC_MSHIFT);
		(*tt.tt_putc)(c & WWC_CMASK);
		(*tt.tt_setinsert)(0);

		p = &wwos[row][wwncol];
		q = p - 1;
		for (i = wwncol - col - 1; --i >= 0;)
			*--p = *--q;
		q->c_w = c;
	}
}
