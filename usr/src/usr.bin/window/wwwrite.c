#ifndef lint
static	char *sccsid = "@(#)wwwrite.c	3.6 83/08/19";
#endif

#include "ww.h"

wwwrite(w, p, n)
register struct ww *w;
register char *p;
int n;
{
	char c;
	int oldn = n;
	char hascursor = 0;

	if (w == 0)
		return -1;
	wwnwrite++;
	wwnwritec += n;
	if (hascursor = w->ww_hascursor)
		wwcursor(w, 0);
	while (--n >= 0) {
		c = *p++ & 0x7f;
		switch (w->ww_wstate) {
		case 0:
			if (!ISCTRL(c)) {
				int i, j;
				register union ww_char *bp;
				union ww_char *bq;
				register union ww_char *ns;
				register char *smap;
				register char *win;
				char *touched;

				if (w->ww_insert) {
					wwinschar(w, w->ww_scroll + w->ww_cur.r,
						w->ww_cur.c, c);
					goto right;
				}
				i = w->ww_w.nc - w->ww_cur.c - 1;
				bp = bq = &w->ww_buf[w->ww_scroll+w->ww_cur.r]
					[w->ww_cur.c];
				bp++->c_w = c | w->ww_modes << WWC_MSHIFT;
				while (n > 0 && --i >= 0 && !ISCTRL(*p)) {
					n--;
					bp++->c_w = *p++ & 0x7f
						| w->ww_modes << WWC_MSHIFT;
				}
				win = &w->ww_win[w->ww_cur.r][w->ww_cur.c];
				i = wwcurrow(w);
				j = wwcurcol(w);
				smap = &wwsmap[i][j];
				ns = &wwns[i][j];
				touched = &wwtouched[i];
				j = i = bp - bq;
				bp = bq;
				while (--i >= 0) {
					if (*smap++ == w->ww_index) {
						*touched = 1;
						ns++->c_w = bp++->c_w
							^ *win++ << WWC_MSHIFT;
					} else {
						ns++;
						bp++;
						win++;
					}
				}
				if ((w->ww_cur.c += j) >= w->ww_w.nc) {
					w->ww_cur.c = 0;
					goto lf;
				}
				break;
			}
			switch (c) {
			case '\n':
				if (w->ww_mapnl)
					w->ww_cur.c = 0;
		lf:
				if (++w->ww_cur.r >= w->ww_w.nr) {
					w->ww_cur.r = w->ww_w.nr - 1;
					if (w->ww_scroll + w->ww_w.nr
					    < w->ww_nline)
						wwscroll(w, 1);
					else
						wwdelline(w, 0);
				}
				break;
			case '\t':
				w->ww_cur.c |= 7;
		right:
				if (++w->ww_cur.c >= w->ww_w.nc) {
					w->ww_cur.c = 0;
					goto lf;
				}
				break;
			case '\b':
				if (--w->ww_cur.c < 0) {
					w->ww_cur.c = w->ww_w.nc - 1;
					goto up;
				}
				break;
			case '\r':
				w->ww_cur.c = 0;
				break;
			case CTRL(g):
				wwbell();
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
		up:
				if (--w->ww_cur.r < 0) {
					w->ww_cur.r = 0;
					if (w->ww_scroll > 0)
						wwscroll(w, -1);
					else
						wwinsline(w, 0);
				}
				break;
			case 'B':
				goto lf;
			case 'C':
				goto right;
			case 'E':
				w->ww_scroll = 0;
				w->ww_cur.c = w->ww_cur.r = 0;
				wwclreos(w, 0, 0);
				break;
			case 'H':
				w->ww_cur.c = w->ww_cur.r = 0;
				break;
			case 'J':
				wwclreos(w, w->ww_scroll + w->ww_cur.r,
					w->ww_cur.c);
				break;
			case 'K':
				wwclreol(w, w->ww_scroll + w->ww_cur.r,
					w->ww_cur.c);
				break;
			case 'L':
				wwinsline(w, w->ww_scroll + w->ww_cur.r);
				break;
			case 'M':
				wwdelline(w, w->ww_scroll + w->ww_cur.r);
				break;
			case 'N':
				wwdelchar(w, w->ww_scroll + w->ww_cur.r,
					w->ww_cur.c);
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
			w->ww_cur.r = (c - ' ') % w->ww_w.nr;
			w->ww_wstate++;
			break;
		case 3:
			w->ww_cur.c = (c - ' ') % w->ww_w.nc;
			w->ww_wstate = 0;
			break;
		}
	}
	if (hascursor)
		wwcursor(w, 1);
	return oldn - n;
}
