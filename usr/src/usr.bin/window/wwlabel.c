#ifndef lint
static	char *sccsid = "@(#)wwlabel.c	3.5 83/08/19";
#endif

#include "ww.h"

/*
 * Label window w on f,
 * at 1 line above w and 'where' columns from it's left edge.
 * Gross, but it works.
 */
wwlabel(w, f, where, l, mode)
struct ww *w;
struct ww *f;
char *l;
{
	int i;
	register j;
	register char *p;
	register char *win;
	register union ww_char *buf;
	register union ww_char *ns;
	register char *fmap;
	char *touched;

	if (f->ww_fmap == 0)
		return -1;

	i = w->ww_w.t - 1 - f->ww_w.t;
	if (i < 0)
		return -1;
	j = w->ww_w.l + where - f->ww_w.l;
	if (j < 0)
		j = 0;
	win = &f->ww_win[i][j];
	buf = &f->ww_buf[f->ww_scroll + i][j];
	fmap = &f->ww_fmap[i][j];

	i += f->ww_w.t;
	j += f->ww_w.l;
	ns = &wwns[i][j];
	touched = &wwtouched[i];

	j = MIN(w->ww_w.r, f->ww_w.r) - j;
	for (; j > 0 && *l;)
		for (p = unctrl(*l++); j > 0 && *p; j--) {
			/* can't label if not already framed */
			if (*win & WWM_GLS) {
				p++;
				buf++;
				ns++;
				win++;
				fmap++;
			} else if (*win & WWM_COV) {
				buf++->c_w = mode << WWC_MSHIFT | *p++;
				*fmap++ |= WWF_LABEL;
				ns++;
				win++;
			} else {
				*touched = 1;
				ns++->c_w = (buf++->c_w
					= mode << WWC_MSHIFT | *p++)
						^ *win++ << WWC_MSHIFT;
				*fmap++ |= WWF_LABEL;
			}
		}

	return 0;
}
