/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwwrite.c	3.33 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"
#include "char.h"

#define UPDATE() \
	if (!w->ww_noupdate && w->ww_cur.r >= 0 && w->ww_cur.r < wwnrow && \
	    wwtouched[w->ww_cur.r]) \
		wwupdate1(w->ww_cur.r, w->ww_cur.r + 1)

/*
 * To support control character expansion, we save the old
 * p and q values in r and s, and point p at the beginning
 * of the expanded string, and q at some safe place beyond it
 * (p + 10).  At strategic points in the loops, we check
 * for (r && !*p) and restore the saved values back into
 * p and q.  Essentially, we implement a stack of depth 2,
 * to avoid recursion, which might be a better idea.
 */
wwwrite(w, p, n)
register struct ww *w;
register char *p;
int n;
{

#ifdef lint
	s = 0;			/* define it before possible use */
#endif
	if (w == 0 || w->ww_win == 0)
	if (hascursor = w->ww_hascursor)
		wwcursor(w, 0);
	while (n-- > 0) {
			}
		chklf:
			if (w->ww_cur.c >= w->ww_w.r)
				goto crlf;
		} else switch (w->ww_wstate) {
		case 0:
			switch (*p++) {
			case '\n':
				Wputc(c, w->ww_win);
				if (w->ww_refresh)
					Wrefresh(1);
				break;
			case '\b':
			case '\r':
				break;
			case ctrl('['):
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
				Wcurup(w->ww_win, 1);
				break;
			case 'B':
				Wcurdown(w->ww_win, 1);
				break;
			case 'C':
				Wcurright(w->ww_win, 1);
				break;
			case 'E':
				w->ww_buf -= w->ww_w.t - w->ww_b.t;
				break;
			case 'H':
				UPDATE();
				WWcursor(w->ww_win, 0, 0);
				break;
			case 'J':
				Wclear(w->ww_win, 0);
				break;
			case 'K':
				Wclearline(w->ww_win, 0);
				break;
			case 'L':
				UPDATE();
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
			case 'P':
				wwinschar(w, w->ww_cur.r, w->ww_cur.c, ' ', 0);
				break;
			case 'X':
				wwupdate();
				break;
			case 'Y':
				UPDATE();
				w->ww_wstate = 2;
				break;
			case 'Z':
				wwupdate();
				xxflush(0);
				break;
			case 's':
				w->ww_wstate = 4;
				break;
			case 'r':
				w->ww_wstate = 5;
				break;
			}
			break;
		case 2:
			WWcursor(w->ww_win, (c - ' ') % w->ww_i.nrow,
				w->ww_win->w_cursor.col);
			w->ww_cur.r = w->ww_w.t +
				(unsigned)(*p++ - ' ') % w->ww_w.nr;
			w->ww_wstate = 3;
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
