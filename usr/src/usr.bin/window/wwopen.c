#ifndef lint
static	char *sccsid = "@(#)wwopen.c	3.9 83/08/26";
#endif

#include "ww.h"

struct ww *
wwopen(flags, nrow, ncol, row, col, nline)
{
	register struct ww *w = 0;
	register i, j;
	char m;
	short nvis;

	w = (struct ww *)calloc(sizeof (struct ww), 1);
	if (w == 0) {
		wwerrno = WWE_NOMEM;
		goto bad;
	}

	for (i = 0; i < NWW && wwindex[i] != 0; i++)
		;
	if (i >= NWW) {
		wwerrno = WWE_TOOMANY;
		goto bad;
	}
	w->ww_index = i;

	if ((w->ww_w.nr = nrow) <= 0
	    || (w->ww_w.nc = ncol) <= 0
	    || (w->ww_w.l = col) < 0
	    || (w->ww_w.r = col + ncol) > wwncol
	    || (w->ww_w.t = row) < 0
	    || (w->ww_w.b = row + nrow) > wwnrow) {
		wwerrno = WWE_SIZE;
		goto bad;
	}
	w->ww_nline = MAX(nline, w->ww_w.nr);

	w->ww_pty = w->ww_tty = -1;	/* closing by mistake is still safe */
	if (flags & WWO_PTY) {
		w->ww_haspty = 1;
		if (wwgetpty(w) < 0)
			goto bad;
		if (wwsettty(w->ww_pty, &wwwintty) < 0)
			goto bad;
	}

	w->ww_win = wwalloc(w->ww_w.nr, w->ww_w.nc, sizeof (char));
	if (w->ww_win == 0)
		goto bad;
	m = 0;
	if (flags & WWO_GLASS)
		m |= WWM_GLS;
	if (flags & WWO_REVERSE)
		m |= WWM_REV;
	for (i = 0; i < w->ww_w.nr; i++)
		for (j = 0; j < w->ww_w.nc; j++)
			w->ww_win[i][j] = m;
	
	w->ww_cov = wwalloc(w->ww_w.nr, w->ww_w.nc, sizeof (char));
	if (w->ww_cov == 0)
		goto bad;
	for (i = 0; i < w->ww_w.nr; i++)
		for (j = 0; j < w->ww_w.nc; j++)
			w->ww_cov[i][j] = WWX_NOBODY;

	if (flags & WWO_FRAME) {
		w->ww_fmap = wwalloc(w->ww_w.nr, w->ww_w.nc, sizeof (char));
		if (w->ww_fmap == 0)
			wwerrno = WWE_NOMEM;
		for (i = 0; i < wwnrow; i++)
			for (j = 0; j < wwncol; j++)
				w->ww_fmap[i][j] = 0;
	}
	
	w->ww_buf = (union ww_char **)
		wwalloc(w->ww_nline, w->ww_w.nc, sizeof (union ww_char));
	if (w->ww_buf == 0)
		goto bad;
	for (i = 0; i < w->ww_nline; i++)
		for (j = 0; j < w->ww_w.nc; j++)
			w->ww_buf[i][j].c_w = ' ';

	w->ww_nvis = (short *)malloc((unsigned) w->ww_w.nr * sizeof (short));
	if (w->ww_nvis == 0) {
		wwerrno = WWE_NOMEM;
		goto bad;
	}
	nvis = m ? 0 : w->ww_w.nc;
	for (i = 0; i < w->ww_w.nr; i++)
		w->ww_nvis[i] = nvis;

	w->ww_state = WWS_INITIAL;
	return wwindex[w->ww_index] = w;
bad:
	if (w != 0) {
		if (w->ww_win != 0)
			wwfree(w->ww_win);
		if (w->ww_cov != 0)
			wwfree(w->ww_cov);
		if (w->ww_fmap != 0)
			wwfree(w->ww_fmap);
		if (w->ww_buf != 0)
			wwfree((char **)w->ww_buf);
		if (w->ww_nvis != 0)
			free((char *)w->ww_nvis);
		(void) close(w->ww_tty);
		(void) close(w->ww_pty);
		free((char *)w);
	}
	return 0;
}
