#ifndef lint
static	char *sccsid = "@(#)wwwrite.c	3.10 83/09/14";
#endif

#include "ww.h"

wwwrite(w, p, n)
register struct ww *w;
register char *p;
int n;
{
	char hascursor;

	wwnwrite++;
	wwnwritec += n;
	if (hascursor = w->ww_hascursor)
		wwcursor(w, 0);
	while (n > 0) {
		if (w->ww_wstate == 0 && !ISCTRL(*p)) {
			register i;
			int crow, ccol;
			register union ww_char *bp;
			union ww_char *bq;

			if (w->ww_insert) {
				n--;
				wwinschar(w, w->ww_scroll + w->ww_cur.r,
					w->ww_cur.c,
					*p++ | w->ww_modes << WWC_MSHIFT);
				goto right;
			}

			bp = bq = &w->ww_buf[w->ww_scroll+w->ww_cur.r]
				[w->ww_cur.c];
			if ((i = w->ww_w.nc - w->ww_cur.c) > n)
				i = n;
			while (--i >= 0 && !ISCTRL(*p))
				bp++->c_w = *p++ | w->ww_modes << WWC_MSHIFT;
			i = bp - bq;
			n -= i;
			bp = bq;

			crow = wwcurrow(w);
			ccol = wwcurcol(w);
			if (ccol < w->ww_i.l) {
				bp += w->ww_i.l - ccol;
				ccol = w->ww_i.l;
			}
			w->ww_cur.c += i;
			if (crow >= w->ww_i.t && crow < w->ww_i.b) {
				register union ww_char *ns;
				register char *smap;
				register char *win;
				char *touched;

				win = &w->ww_win[w->ww_cur.r][ccol - w->ww_w.l];
				smap = &wwsmap[crow][ccol];
				ns = &wwns[crow][ccol];
				touched = &wwtouched[crow];
				if (i > w->ww_i.r - ccol)
					i = w->ww_i.r - ccol;
				while (--i >= 0)
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
			if (w->ww_cur.c >= w->ww_w.nc) {
				w->ww_cur.c = 0;
				goto lf;
			}
			continue;
		}
		n--;
		switch (w->ww_wstate) {
		case 0:
			switch (*p++) {
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
			switch (*p++) {
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
			w->ww_cur.r = (*p++ - ' ') % w->ww_w.nr;
			w->ww_wstate++;
			break;
		case 3:
			w->ww_cur.c = (*p++ - ' ') % w->ww_w.nc;
			w->ww_wstate = 0;
			break;
		}
	}
	if (hascursor)
		wwcursor(w, 1);
	return 0;
}
