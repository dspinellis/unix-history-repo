#ifndef lint
static	char *sccsid = "@(#)wwclreol.c	3.8 83/09/14";
#endif

#include "ww.h"
#include "tt.h"

wwclreol(w, line, col)
struct ww *w;
{
	wwclreol1(w, line, col, 0);
}

/*
 * Clear w to the end of line.
 * If cleared is true, then the screen line has already been cleared
 * previously.
 */
wwclreol1(w, line, col, cleared)
register struct ww *w;
int line, col;
char cleared;
{
	register i;
	int row = line - w->ww_scroll;
	int srow = w->ww_w.t + row;
	int scol = w->ww_w.l + col;
	int nblank, ncleared;

	/*
	 * Clear the buffer right off
	 */
	{
		register union ww_char *buf;

		buf = &w->ww_buf[line][col]; 
		for (i = w->ww_w.nc - col; --i >= 0;)
			buf++->c_w = ' ';
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
	 * Now find out how much is actually cleared, and fix wwns.
	 */
	{
		register union ww_char *s;
		register char *smap, *win;
		register char *touched;

		smap = &wwsmap[srow][scol];
		s = &wwns[srow][scol];
		touched = &wwtouched[srow];
		win = &w->ww_win[row][col];
		ncleared = nblank = 0;

		for (i = w->ww_i.r - scol; --i >= 0;) {
			if (*smap++ != w->ww_index) {
				if (s++->c_w == ' ')
					nblank++;
				win++;
				continue;
			}
			ncleared++; 
			*touched = 1;
			if (*win == 0) {
				nblank++;
				s++->c_w = ' ';
				win++;
			} else
				s++->c_w = ' ' | *win++ << WWC_MSHIFT;
		}
	}

	/*
	 * Can/Should we use clear eol?
	 */
	if (!cleared && tt.tt_clreol != 0
	    && ncleared > wwncol - scol - nblank
	    && nblank > (wwncol - scol) / 2) {
		register union ww_char *s;

		/* clear to the end */
		(*tt.tt_move)(srow, scol);
		(*tt.tt_clreol)();
		s = &wwos[srow][scol];
		for (i = wwncol - scol; --i >= 0;)
			s++->c_w = ' ';
	}
}
