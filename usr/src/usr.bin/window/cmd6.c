#ifndef lint
static	char *sccsid = "@(#)cmd6.c	2.1 83/07/30";
#endif

#include "defs.h"

struct ww *openwin();
struct ww *doopen();

doscroll(dir)
int dir;
{
	register Win *W = selwin->ww_win;
	register brow = W->w_bstart.row;

	brow += dir * selwin->ww_i.nrow / 2;
	if (brow < 0)
		brow = 0;
	else if (brow + selwin->ww_w.nrow > W->w_textbuf->b_nrows)
		brow = W->w_textbuf->b_nrows - selwin->ww_w.nrow;
	if (brow != W->w_bstart.row)
		Wrelscroll(W, brow - W->w_bstart.row, 0, 1);
}
