#ifndef lint
static	char *sccsid = "@(#)wwwrite.c	2.1.1.1 83/08/09";
#endif

#include "ww.h"

int wwnwrite;
int wwnwritec;

wwwrite(w, p, n)
register struct ww *w;
register char *p;
register n;
{
	register char c;
	int oldn = n;

	if (w == 0)
		return -1;
	wwnwrite++;
	wwnwritec += n;
	while (--n >= 0) {
		c = *p++ & 0x7f;
		switch (w->ww_wstate) {
		case 0:
			if (c >= ' ' && c < 0x7f) {
				register i, j, cc;

				if (w->ww_insert)
					wwinschar(w, 1);
				cc = w->ww_buf[w->ww_scroll + w->ww_cur.r]
					[w->ww_cur.c].c_w = c;
				i = wwcurrow(w);
				j = wwcurcol(w);
				if (wwsmap[i][j] == w->ww_index) {
					cc = wwns[i][j].c_w = cc
						^ w->ww_win[w->ww_cur.r]
						[w->ww_cur.c] << WWC_MSHIFT;
				}
		right:
				if (++w->ww_cur.c >= w->ww_w.nc) {
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
					wwdelline(w, 0);
				}
				break;
			case '\t':
				w->ww_cur.c |= 7;
				goto right;
				break;
			case '\b':
				if (--w->ww_cur.c < 0)
					w->ww_cur.c = 0;
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
				if (--w->ww_cur.r < 0)
					w->ww_cur.r = 0;
				break;
			case 'B':
				goto lf;
			case 'C':
				goto right;
			case 'E':
				w->ww_cur.c = w->ww_cur.r = 0;
				wwclreos(w);
				break;
			case 'H':
				w->ww_cur.c = w->ww_cur.r = 0;
				break;
			case 'J':
				wwclreos(w);
				break;
			case 'K':
				wwclreol(w, w->ww_scroll + w->ww_cur.r,
					w->ww_cur.c, 0);
				break;
			case 'L':
				wwinsline(w);
				break;
			case 'M':
				wwdelline(w, w->ww_scroll + w->ww_cur.r);
				break;
			case 'N':
				wwdelchar(w);
				break;
			case 'O':
				w->ww_insert = 0;
				break;
			case 'Y':
				w->ww_wstate = 2;
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
	return oldn - n;
}
