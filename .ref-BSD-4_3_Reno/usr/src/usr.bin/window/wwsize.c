/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwsize.c	3.9 (Berkeley) 6/6/90";
#endif /* not lint */

#include "ww.h"
#ifdef POSIX_TTY
#include <sys/ioctl.h>
#endif

/*
 * Resize a window.  Should be unattached.
 */
wwsize(w, nrow, ncol)
register struct ww *w;
{
	register i, j;
	int nline;
	union ww_char **buf = 0;
	char **win = 0;
	short *nvis = 0;
	char **fmap = 0;
	char m;

	/*
	 * First allocate new buffers.
	 */
	win = wwalloc(w->ww_w.t, w->ww_w.l, nrow, ncol, sizeof (char));
	if (win == 0)
		goto bad;
	if (w->ww_fmap != 0) {
		fmap = wwalloc(w->ww_w.t, w->ww_w.l, nrow, ncol, sizeof (char));
		if (fmap == 0)
			goto bad;
	}
	if (nrow > w->ww_b.nr || ncol > w->ww_b.nc) {
		nline = MAX(w->ww_b.nr, nrow);
		buf = (union ww_char **) wwalloc(w->ww_b.t, w->ww_b.l,
			nline, ncol, sizeof (union ww_char));
		if (buf == 0)
			goto bad;
	}
	nvis = (short *)malloc((unsigned) nrow * sizeof (short));
	if (nvis == 0) {
		wwerrno = WWE_NOMEM;
		goto bad;
	}
	nvis -= w->ww_w.t;
	/*
	 * Copy text buffer.
	 */
	if (buf != 0) {
		int b, r;

		b = w->ww_b.t + nline;
		r = w->ww_b.l + ncol;
		if (ncol < w->ww_b.nc)
			for (i = w->ww_b.t; i < w->ww_b.b; i++)
				for (j = w->ww_b.l; j < r; j++)
					buf[i][j] = w->ww_buf[i][j];
		else
			for (i = w->ww_b.t; i < w->ww_b.b; i++) {
				for (j = w->ww_b.l; j < w->ww_b.r; j++)
					buf[i][j] = w->ww_buf[i][j];
				for (; j < r; j++)
					buf[i][j].c_w = ' ';
			}
		for (; i < b; i++)
			for (j = w->ww_b.l; j < r; j++)
				buf[i][j].c_w = ' ';
	}
	/*
	 * Now free the old stuff.
	 */
	wwfree((char **)w->ww_win, w->ww_w.t);
	w->ww_win = win;
	if (buf != 0) {
		wwfree((char **)w->ww_buf, w->ww_b.t);
		w->ww_buf = buf;
	}
	if (w->ww_fmap != 0) {
		wwfree((char **)w->ww_fmap, w->ww_w.t);
		w->ww_fmap = fmap;
	}
	free((char *)(w->ww_nvis + w->ww_w.t));
	w->ww_nvis = nvis;
	/*
	 * Set new sizes.
	 */
		/* window */
	w->ww_w.b = w->ww_w.t + nrow;
	w->ww_w.r = w->ww_w.l + ncol;
	w->ww_w.nr = nrow;
	w->ww_w.nc = ncol;
		/* text buffer */
	if (buf != 0) {
		w->ww_b.b = w->ww_b.t + nline;
		w->ww_b.r = w->ww_b.l + ncol;
		w->ww_b.nr = nline;
		w->ww_b.nc = ncol;
	}
		/* scroll */
	if ((i = w->ww_b.b - w->ww_w.b) < 0 ||
	    (i = w->ww_cur.r - w->ww_w.b + 1) > 0) {
		w->ww_buf += i;
		w->ww_b.t -= i;
		w->ww_b.b -= i;
		w->ww_cur.r -= i;
	}
		/* interior */
	w->ww_i.b = MIN(w->ww_w.b, wwnrow);
	w->ww_i.r = MIN(w->ww_w.r, wwncol);
	w->ww_i.nr = w->ww_i.b - w->ww_i.t;
	w->ww_i.nc = w->ww_i.r - w->ww_i.l;
	/*
	 * Initialize new buffers.
	 */
		/* window */
	m = 0;
	if (w->ww_oflags & WWO_GLASS)
		m |= WWM_GLS;
	if (w->ww_oflags & WWO_REVERSE)
		m |= WWM_REV;
	for (i = w->ww_w.t; i < w->ww_w.b; i++)
		for (j = w->ww_w.l; j < w->ww_w.r; j++)
			w->ww_win[i][j] = m;
		/* frame map */
	if (fmap != 0)
		for (i = w->ww_w.t; i < w->ww_w.b; i++)
			for (j = w->ww_w.l; j < w->ww_w.r; j++)
				w->ww_fmap[i][j] = 0;
		/* visibility */
	j = m ? 0 : w->ww_w.nc;
	for (i = w->ww_w.t; i < w->ww_w.b; i++)
		w->ww_nvis[i] = j;
	/*
	 * Put cursor back.
	 */
	if (w->ww_hascursor) {
		w->ww_hascursor = 0;
		wwcursor(w, 1);
	}
	/*
	 * Fool with pty.
	 */
	if (w->ww_ispty && w->ww_pty >= 0) {
		struct winsize winsize;

		winsize.ws_row = nrow;
		winsize.ws_col = ncol;
		winsize.ws_xpixel = winsize.ws_ypixel = 0;
		(void) ioctl(w->ww_pty, TIOCSWINSZ, (char *)&winsize);
	}
	return 0;
bad:
	if (win != 0)
		wwfree(win, w->ww_w.t);
	if (fmap != 0)
		wwfree(fmap, w->ww_w.t);
	if (buf != 0)
		wwfree((char **)buf, w->ww_b.t);
	if (nvis != 0)
		free((char *)(nvis + w->ww_w.t));
	return -1;
}
