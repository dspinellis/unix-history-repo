#ifndef lint
static	char *sccsid = "@(#)wwredrawwin.c	3.7 83/12/02";
#endif

#include "ww.h"

wwredrawwin1(w, row1, row2, offset)
register struct ww *w;
int row1, row2, offset;
{
	int row;
	register col;
	register char *smap;
	register union ww_char *buf;
	register char *win;
	register union ww_char *ns;
	char *touched;

	touched = &wwtouched[row1];
	for (row = row1; row < row2; row++, touched++) {
		col = w->ww_i.l;
		ns = wwns[row];
		smap = &wwsmap[row][col];
		buf = w->ww_buf[row + offset];
		win = w->ww_win[row];
		for (; col < w->ww_i.r; col++)
			if (*smap++ == w->ww_index) {
				*touched = 1;
				ns[col].c_w =
					buf[col].c_w ^ win[col] << WWC_MSHIFT;
			}
	}
}
