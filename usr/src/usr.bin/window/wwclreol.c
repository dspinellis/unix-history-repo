#ifndef lint
static	char *sccsid = "@(#)wwclreol.c	3.10 83/12/02";
#endif

#include "ww.h"
#include "tt.h"

/*
 * Clear w to the end of line.
 * If cleared is true, then the screen line has already been cleared
 * previously.
 */
wwclreol1(w, row, col, cleared)
register struct ww *w;
int row, col;
char cleared;
{
	register i;
	int nblank, ncleared;

	/*
	 * Clear the buffer right off
	 */
	{
		register union ww_char *buf;

		buf = &w->ww_buf[row][col]; 
		for (i = w->ww_b.r - col; --i >= 0;)
			buf++->c_w = ' ';
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
	 * Now find out how much is actually cleared, and fix wwns.
	 */
	{
		register union ww_char *s;
		register char *smap, *win;
		register char *touched;

		i = col;
		smap = &wwsmap[row][i];
		s = &wwns[row][i];
		win = w->ww_win[row];
		touched = &wwtouched[row];
		ncleared = nblank = 0;

		for (; i < w->ww_i.r; i++) {
			if (*smap++ != w->ww_index) {
				if (s++->c_w == ' ')
					nblank++;
				continue;
			}
			ncleared++; 
			*touched = 1;
			if (win[i] == 0) {
				nblank++;
				s++->c_w = ' ';
			} else
				s++->c_w = ' ' | win[i] << WWC_MSHIFT;
		}
	}

	/*
	 * Can/Should we use clear eol?
	 */
	if (!cleared && tt.tt_clreol != 0
	    && ncleared > wwncol - col - nblank
	    && nblank > (wwncol - col) / 2) {
		register union ww_char *s;

		/* clear to the end */
		(*tt.tt_move)(row, col);
		(*tt.tt_clreol)();
		s = &wwos[row][col];
		for (i = wwncol - col; --i >= 0;)
			s++->c_w = ' ';
	}
}
