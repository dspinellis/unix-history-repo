#ifndef lint
static	char *sccsid = "@(#)wwwrite.c	3.9 83/08/26";
#endif

#include "ww.h"

wwwrite(w, p, n)
register struct ww *w;
register char *p;
int n;
{
	char c;

	if (w == 0 || w->ww_win == 0)
	wwnwrite++;
	wwnwritec += n;
	if (hascursor = w->ww_hascursor)
		wwcursor(w, 0);
	while (n-- > 0) {
		c = *p++ & 0x7f;
		switch (w->ww_wstate) {
		case 0:
				if (w->ww_insert) {
					Winschars(w->ww_win, 1);
				Wputc(c, w->ww_win);
				break;
			}
			switch (c) {
			case '\n':
				Wputc(c, w->ww_win);
				if (w->ww_refresh)
					Wrefresh(1);
				break;
			case '\t':
			case '\b':
			case '\r':
			case CTRL(g):
				Wputc(c, w->ww_win);
				break;
			case CTRL([):
				w->ww_wstate = 1;
				break;
			}
			break;
		case 1:
			w->ww_wstate = 0;
			switch (c) {
			case '@':
				w->ww_insert = 1;
				break;
			case 'A':
				Wcurup(w->ww_win, 1);
				break;
			case 'B':
				Wcurdown(w->ww_win, 1);
				break;
			case 'C':
				Wcurright(w->ww_win, 1);
				break;
			case 'E':
				w->ww_scroll = 0;
				WWcursor(w->ww_win, 0, 0);
				Wclear(w->ww_win, 2);
				/* always refresh */
				Wrefresh(1);
				break;
			case 'H':
				WWcursor(w->ww_win, 0, 0);
				break;
			case 'J':
				Wclear(w->ww_win, 0);
				break;
			case 'K':
				Wclearline(w->ww_win, 0);
				break;
			case 'L':
				Winslines(w->ww_win, 1);
				if (w->ww_refresh)
					Wrefresh(1);
				break;
			case 'M':
				Wdellines(w->ww_win, 1);
				if (w->ww_refresh)
					Wrefresh(1);
				break;
			case 'N':
				Wdelchars(w->ww_win, 1);
				break;
			case 'O':
				w->ww_insert = 0;
				break;
			case 'Y':
				w->ww_wstate = 2;
				break;
			case 'p':
				w->ww_modes |= WWM_REV;
				break;
			case 'q':
				w->ww_modes &= ~WWM_REV;
				break;
			case 'r':
				w->ww_modes |= WWM_UL;
				break;
			case 's':
				w->ww_modes &= ~WWM_UL;
				break;
			}
			break;
		case 2:
			WWcursor(w->ww_win, (c - ' ') % w->ww_i.nrow,
				w->ww_win->w_cursor.col);
			w->ww_wstate++;
			break;
		case 3:
			WWcursor(w->ww_win, w->ww_win->w_cursor.row,
				(c - ' ') % w->ww_i.ncol);
			w->ww_wstate = 0;
			break;
		}
	}
	if (hascursor)
		wwcursor(w, 1);
}
