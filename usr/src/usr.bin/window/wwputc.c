#ifndef lint
static	char *sccsid = "@(#)wwputc.c	1.1 83/07/12";
#endif

#include "ww.h"

wwputc(c, w)
	register char c;
	register struct ww *w;
{
	char clearline = 0;

	if (w == 0 || w->ww_win == 0)
		return -1;
	w->ww_touched = 1;
	c &= 0x7f;
	switch (w->ww_wstate) {
	case 0:
		if (c >= ' ' && c < 0x7f) {
			mvwaddch(w->ww_win, w->ww_y, w->ww_x, c);
			getyx(w->ww_win, w->ww_y, w->ww_x);
			break;
		}
		switch (c) {
		case CTRL(g):
			/* ??? */
			break;
		case CTRL([):
			w->ww_wstate++;
			break;
		case CTRL(z):
			w->ww_y = w->ww_x = 0;
			wclear(w->ww_win);
			break;
		case '\r':
			w->ww_x = 0;
			break;
		case '\n':
			w->ww_y++;
			clearline++;
			break;
		case '\t':
			w->ww_x += 8 - w->ww_x % 8;
			break;
		case '\b':
			if (--w->ww_x < 0)
				w->ww_x = 0;
			break;
		case CTRL(l):
			w->ww_x++;
			break;
		case CTRL(k):
			if (--w->ww_y < 0)
				w->ww_y = 0;
			break;
		}
		break;
	case 1:
		switch (c) {
		case '=':
			w->ww_wstate++;
			break;
		default:
			w->ww_wstate = 0;
		}
		break;
	case 2:
		w->ww_y = (c - ' ') % w->ww_nrow;
		w->ww_wstate++;
		break;
	case 3:
		w->ww_x = (c - ' ') % w->ww_ncol;
		w->ww_wstate = 0;
		break;
	}
	if (w->ww_x >= w->ww_ncol) {
		w->ww_x = 0;
		w->ww_y++;
		clearline++;
	}
	if (w->ww_y >= w->ww_nrow)
		w->ww_y = 0;
	if (clearline) {
		wmove(w->ww_win, w->ww_y, w->ww_x);
		wclrtoeol(w->ww_win);
	}
}

wwflush(w)
register struct ww *w;
{
	wmove(w->ww_win, w->ww_y, w->ww_x);
	wrefresh(w->ww_win);
	w->ww_touched = 0;
}

wwflushall()
{
	register struct ww *w;

	for (w = _wwhead; w; w = w->ww_next)
		if (w != _wwcurrent && w->ww_touched) {
			wrefresh(w->ww_win);
			w->ww_touched = 0;
		}
	wwflush(_wwcurrent);
}
