#ifndef lint
static	char *sccsid = "@(#)cmd5.c	2.1.1.1 83/08/09";
#endif

#include "defs.h"

/*
c_scroll(dir)
int dir;
{
	register brow = selwin->ww_scroll;

	brow += dir * selwin->ww_w.nrow / 2;
	if (brow < 0)
		brow = 0;
	else if (brow + selwin->ww_w.nrow > selwin->ww_nline)
		brow = selwin->ww_nline - selwin->ww_w.nrow;
	if (brow != selwin->ww_scroll) {
		selwin->ww_scroll = brow;
		wwredrawwin(selwin);
	}
}
*/
