#ifndef lint
static	char *sccsid = "@(#)wwwrite.c	3.13 83/09/15";
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
			register union ww_char *bp;
			union ww_char *bq;
			int col;

			if (w->ww_insert) {
				n--;
				wwinschar(w, w->ww_cur.r, w->ww_cur.c,
					*p++ | w->ww_modes << WWC_MSHIFT);
				goto right;
			}

			bp = bq = &w->ww_buf[w->ww_cur.r][w->ww_cur.c];
			if ((i = w->ww_b.r - w->ww_cur.c) > n)
				i = n;
			while (--i >= 0 && !ISCTRL(*p))
				bp++->c_w = *p++ | w->ww_modes << WWC_MSHIFT;

			i = bp - bq;
			n -= i;
			col = w->ww_cur.c;
			w->ww_cur.c += i;
			bp = bq;
			if (col < w->ww_i.l) {
				/* use col as a temporary */
				col = w->ww_i.l - col;
				bp += col;
				i -= col;
				col = w->ww_i.l;
			}
			if (i > w->ww_i.r - col)
				i = w->ww_i.r - col;

			if (w->ww_cur.r >= w->ww_i.t && w->ww_cur.r < w->ww_i.b)
			{
				register union ww_char *ns;
				register char *smap;
				register char *win;
				char *touched;

				win = &w->ww_win[w->ww_cur.r][col];
				smap = &wwsmap[w->ww_cur.r][col];
				ns = &wwns[w->ww_cur.r][col];
				touched = &wwtouched[w->ww_cur.r];
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
			if (w->ww_cur.c >= w->ww_w.r) {
				w->ww_cur.c = w->ww_w.l;
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
					w->ww_cur.c = w->ww_w.l;
		lf:
				if (++w->ww_cur.r >= w->ww_w.b) {
					w->ww_cur.r = w->ww_w.b - 1;
					if (w->ww_w.b < w->ww_b.b) {
						(void) wwscroll1(w, w->ww_i.t,
							w->ww_i.b, 1, 0);
						w->ww_buf++;
						w->ww_b.t--;
						w->ww_b.b--;
					} else
						wwdelline(w, w->ww_b.t);
				}
				break;
			case '\t':
				w->ww_cur.c +=
					8 - (w->ww_cur.c - w->ww_w.l & 7);
				if (w->ww_cur.c >= w->ww_w.r) {
					w->ww_cur.c = w->ww_w.l;
					goto lf;
				}
				break;
			case '\b':
				if (--w->ww_cur.c < w->ww_w.l) {
					w->ww_cur.c = w->ww_w.r - 1;
					goto up;
				}
				break;
			case '\r':
				w->ww_cur.c = w->ww_w.l;
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
				if (--w->ww_cur.r < w->ww_w.t) {
					w->ww_cur.r = w->ww_w.t;
					if (w->ww_w.t > w->ww_b.t) {
						(void) wwscroll1(w, w->ww_i.t,
							w->ww_i.b, -1, 0);
						w->ww_buf--;
						w->ww_b.t++;
						w->ww_b.b++;
					} else
						wwinsline(w, w->ww_b.t);
				}
				break;
			case 'B':
				goto lf;
			case 'C':
		right:
				if (++w->ww_cur.c >= w->ww_w.r) {
					w->ww_cur.c = w->ww_w.l;
					goto lf;
				}
				break;
			case 'E':
				w->ww_buf -= w->ww_w.t - w->ww_b.t;
				w->ww_b.t = w->ww_w.t;
				w->ww_b.b = w->ww_b.t + w->ww_b.nr;
				w->ww_cur.r = w->ww_w.t;
				w->ww_cur.c = w->ww_w.l;
				wwclreos(w, w->ww_w.t, w->ww_w.l);
				break;
			case 'H':
				w->ww_cur.r = w->ww_w.t;
				w->ww_cur.c = w->ww_w.l;
				break;
			case 'J':
				wwclreos(w, w->ww_cur.r, w->ww_cur.c);
				break;
			case 'K':
				wwclreol(w, w->ww_cur.r, w->ww_cur.c);
				break;
			case 'L':
				wwinsline(w, w->ww_cur.r);
				break;
			case 'M':
				wwdelline(w, w->ww_cur.r);
				break;
			case 'N':
				wwdelchar(w, w->ww_cur.r, w->ww_cur.c);
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
			w->ww_cur.r = w->ww_w.t + (*p++ - ' ') % w->ww_w.nr;
			w->ww_wstate = 3;
			break;
		case 3:
			w->ww_cur.c = w->ww_w.l + (*p++ - ' ') % w->ww_w.nc;
			w->ww_wstate = 0;
			break;
		}
	}
	if (hascursor)
		wwcursor(w, 1);
	return 0;
}
