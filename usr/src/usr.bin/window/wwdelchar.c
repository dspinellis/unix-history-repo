#ifndef lint
static	char *sccsid = "@(#)wwdelchar.c	3.4 83/08/16";
#endif

#include "ww.h"
#include "tt.h"

wwdelchar(w, line, col)
register struct ww *w;
{
	register i;
	int row = line - w->ww_scroll;
	int nvis;

	{
		register union ww_char *p, *q;

		p = &w->ww_buf[line][col];
		q = p + 1;
		for (i = w->ww_w.nc - col - 1; --i >= 0;)
			*p++ = *q++;
		p->c_w = ' ';
	}
	if (row < 0 || row >= w->ww_w.nr)
		return;
	{
		register union ww_char *buf;
		register char *win;
		register union ww_char *ns;
		register char *smap;
		char *touched;

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
		touched = &wwtouched[row + w->ww_w.t];
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
	col += w->ww_w.l;
	row += w->ww_w.t;
	if (nvis > (wwncol - col) / 2) {
		register union ww_char *p, *q;

		(*tt.tt_move)(row, col);
		(*tt.tt_delchar)();

		p = &wwos[row][col];
		q = p + 1;
		for (i = wwncol - col - 1; --i >= 0;)
			*p++ = *q++;
		p->c_w = ' ';
	}
}
