#ifndef lint
static	char *sccsid = "@(#)wwmisc.c	3.3 83/09/15";
#endif

#include "ww.h"

/*
 * Sufficient but not necessary test for total visibility.
 */
wwvisible(w)
register struct ww *w;
{
	register i;
	register nvis = 0;

	for (i = w->ww_i.t; i < w->ww_i.b; i++)
		nvis += w->ww_nvis[i];
	if (w->ww_hascursor
	    && w->ww_cur.r >= w->ww_i.b && w->ww_cur.r < w->ww_i.t
	    && w->ww_cur.c >= w->ww_i.l && w->ww_cur.c < w->ww_i.r
	    && wwsmap[w->ww_cur.r][w->ww_cur.c] == w->ww_index)
		nvis++;
	return nvis == w->ww_i.nr * w->ww_i.nc;
}

struct ww *wwhead = 0;
struct ww *curwin = 0;

wwsetcurwin(wp)
register struct ww *wp;
{
	curwin = wp;
	Wfront(wp->ww_win);
}

wwhaschildren()
{
	register struct ww *wp;

	for (wp = wwhead; wp; wp = wp->ww_next)
		if (wp->ww_state == WW_HASPROC)
			return 1;
	return 0;
}

struct ww *
wwfind(id)
register id;
{
	register struct ww *w;

	for (w = wwhead; w && w->ww_ident != id; w = w->ww_next)
		;
	return w;
}

char *
unctrl(c)
register char c;
{
	static char buf[5];
	register char *p = buf;

	if (c == DEL) {
		*p++ = '^';
		*p++ = '?';
	} else if (c < ' ') {
		*p++ = '^';
		*p++ = c + '@';
	} else if (c > DEL) {
		*p++ = '\\';
		*p++ = (c >> 6 & 3) + '0';
		*p++ = (c >> 3 & 7) + '0';
		*p++ = (c & 7) + '0';
	} else
		*p++ = c;
	*p = 0;
	return buf;
}
