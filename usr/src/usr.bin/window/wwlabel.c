#ifndef lint
static	char *sccsid = "@(#)wwlabel.c	3.7 83/09/15";
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
		return;

	i = w->ww_w.t - 1;
	if (i < f->ww_i.t || i >= f->ww_i.b)
		return;
	j = w->ww_i.l + where;
	win = &f->ww_win[i][j];
	buf = &f->ww_buf[i][j];
	fmap = &f->ww_fmap[i][j];

	ns = &wwns[i][j];
	touched = &wwtouched[i];

	j = MIN(w->ww_i.r, f->ww_i.r) - j;
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
}
