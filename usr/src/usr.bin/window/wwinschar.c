#ifndef lint
static	char *sccsid = "@(#)wwinschar.c	3.1 83/08/09";
#endif

#include "ww.h"

wwinschar(w, n)
register struct ww *w;
register n;
{
	register i, j;
	int col, row;

	col = w->ww_cur.c + w->ww_w.l;
	row = w->ww_cur.r + w->ww_w.t;
	for (i = col; i < wwncol;) {
		for (j = i; j < wwncol && wwsmap[row][j] == w->ww_index; j++)
			;
		if (j > i) {
			if (j <= i + n) {
				if (j == wwncol) {
					(*tt.tt_clreol)(w);
				} else {
					(*tt.tt_blank)(j);
					(*tt.tt_move)(row, col);
				}
			} else {
				if (j < wwncol) {
					(*tt.tt_move)(row, j - n);
					(*tt.tt_delchar)(n);
					(*tt.tt_move)(row, col);
				}
				(*tt.tt_setinsert)(1);
				(*tt.tt_blank)(n);
			}
		}
		for (i = j; i < wwncol && wwsmap[row][i] != w->ww_index; i++)
			;
	}
	(*tt.tt_setinsert)(0);
}
