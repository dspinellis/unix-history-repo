#ifndef lint
static	char *sccsid = "@(#)wwmisc.c	1.4 83/07/19";
#endif

#include "ww.h"

struct ww *wwhead = 0;
struct ww *curwin = 0;

wwsetcurrent(wp)
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
