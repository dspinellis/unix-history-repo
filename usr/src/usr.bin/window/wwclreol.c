#ifndef lint
static	char *sccsid = "@(#)wwclreol.c	3.1 83/08/09";
#endif

#include "ww.h"

/*
 * Clear w to the end of line.
 * If cleared is true, then the screen line has already been cleared
 * previously.
 */
wwclreol(w, line, col, cleared)
register struct ww *w;
int line;
{
	register i;
	int row = line - w->ww_scroll;
	int srow = w->ww_w.t + row;
	int scol = w->ww_w.l + col;
	register union ww_char *s, *buf;
	register char *smap, *win;
	int nblank, ncleared;

	if (row < 0 || row >= w->ww_w.nr) {
		/* can't even see it, so just clear the buffer */
		buf = &w->ww_buf[line][col]; 
		for (i = w->ww_w.nc - col; --i >= 0;)
			buf++->c_w = ' ';
		return;
	}
	smap = &wwsmap[srow][scol];
	s = &wwns[srow][scol];
	win = &w->ww_win[row][col];
	buf = &w->ww_buf[row][col];
	ncleared = nblank = 0;
	for (i = w->ww_w.nc - col; --i >= 0;) {
		if (*smap++ != w->ww_index) {
			if (s++->c_w == ' ')
				nblank++;
			buf++;
			win++;
			continue;
		}
		ncleared++; 
		buf++->c_w = ' ';
		if (*win == 0) {
			nblank++;
			s++->c_w = ' ';
			win++;
		} else
			s++->c_w = ' ' | *win++ << WWC_MSHIFT;
	}
	if (!cleared && tt.tt_clreol != 0
	    && ncleared > wwncol - scol - nblank
	    && nblank > (wwncol - scol) / 2) {
		/* clear to the end */
		(*tt.tt_move)(srow, scol);
		(*tt.tt_clreol)();
		s = &wwos[srow][scol];
		for (i = w->ww_w.nc - col; --i >= 0;)
			s++->c_w = ' ';
	}
}
