#ifndef lint
static	char *sccsid = "@(#)wwlabel.c	3.2 83/08/12";
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

	if ((i = w->ww_w.t - 1 - f->ww_w.t) < 0)
		return -1;
	j = w->ww_w.l + where;
	j = MAX(j, f->ww_w.l);
	win = &f->ww_win[i][j - f->ww_w.l];
	buf = &f->ww_buf[w->ww_scroll + i][j - f->ww_w.l];
	i += f->ww_w.t;
	ns = &wwns[i][j];
	fmap = &wwfmap[i][j];
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
				ns++->c_w = (buf++->c_w
					= mode << WWC_MSHIFT | *p++)
						^ *win++ << WWC_MSHIFT;
				*fmap++ |= WWF_LABEL;
			}
		}

	return 0;
}
