#ifndef lint
static	char *sccsid = "@(#)wwdelline.c	3.1 83/08/09";
#endif

#include "ww.h"

wwdelline(w, line)
register struct ww *w;
int line;
{
	register i;
	int j;
	int row = line - w->ww_scroll;
	int srow = MAX(row, 0) + w->ww_w.t;
	int startrow, endrow;
	int nvis;
	int nvismax;
	int deleted = 0;

	/*
	 * Fix the buffer.
	 * But leave clearing the last line for wwclreol().
	 */
	{
		register union ww_char *tmp;
		register union ww_char **cpp, **cqq;
		cpp = &w->ww_buf[line];
		cqq = cpp + 1;
		tmp = *cpp;
		for (i = w->ww_nline - line - 1; --i >= 0;)
			*cpp++ = *cqq++;
		*cpp = tmp;
	}
	/*
	 * See how many lines on the screen are affected.
	 */
	for (i = MAX(0, row); i < w->ww_w.nr && w->ww_nvis[i] == 0; i++)
		;
	if ((startrow = i + w->ww_w.t) == w->ww_w.b) {
		endrow = startrow - 1;
		goto out;
	}
	for (i = w->ww_w.nr - 1; i > 0 && w->ww_nvis[i] == 0; i--)
		;
	if ((endrow = i + w->ww_w.t) == startrow)
		goto out;		/* just one line is easy */
	/*
	 * See how much of this window is visible.
	 */
	nvismax = wwncol * (endrow - startrow + 1);
	nvis = 0;
	for (i = startrow; i <= endrow; i++)
		nvis += w->ww_nvis[i - w->ww_w.t];
	/*
	 * If it's a good idea to use delete and insert line
	 * and the terminal can, then do it.
	 */
	if (nvis > nvismax / 2 && tt.tt_delline && tt.tt_insline) {
		register union ww_char *tmp;
		register union ww_char **cpp, **cqq;

		(*tt.tt_move)(startrow, 0);
		(*tt.tt_delline)();
		if (endrow != wwnrow - 1) {
			(*tt.tt_move)(endrow, 0);
			(*tt.tt_insline)();
		}
		/*
		 * Fix up the old screen.
		 */
		cpp = &wwos[startrow];
		cqq = cpp + 1;
		tmp = *cpp;
		for (i = endrow - startrow; --i >= 0;)
			*cpp++ = *cqq++;
		*cpp = tmp;
		for (i = wwncol; --i >= 0;)
			tmp++->c_w = ' ';
		deleted++;
	}
	/*
	 * Fix the new screen.
	 */
	if (nvis == nvismax) {
		register union ww_char *tmp;
		register union ww_char **cpp, **cqq;

		cpp = &wwns[startrow];
		cqq = cpp + 1;
		tmp = *cpp;
		for (i = endrow - startrow; --i >= 0;)
			*cpp++ = *cqq++;
		*cpp = tmp;
		i = startrow - 1;
	} else {
out:
		/* wwclreol() will do the last line */
		if ((i = endrow) == w->ww_nline - w->ww_scroll + w->ww_w.t - 1)
			i--;
	}
	for (; i >= srow; i--) {
		register char *smap;
		register union ww_char *buf;
		register char *win;
		register union ww_char *ns;

		ns = &wwns[i][w->ww_w.l];
		smap = &wwsmap[i][w->ww_w.l];
		buf = w->ww_buf[w->ww_scroll + i - w->ww_w.t];
		win = w->ww_win[i - w->ww_w.t];
		for (j = w->ww_w.nc; --j >= 0;)
			if (*smap++ != w->ww_index)
				win++, ns++, buf++;
			else
				ns++->c_w = buf++->c_w
					^ *win++ << WWC_MSHIFT;
	}
	wwclreol(w, w->ww_nline - 1, 0, deleted);
}
