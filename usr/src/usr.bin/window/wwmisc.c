#ifndef lint
static	char *sccsid = "@(#)wwmisc.c	1.2 83/07/17";
#endif

#include "ww.h"

struct ww *wwhead = 0;
struct ww *curwin = 0;

wwsetcurrent(wp)
register struct ww *wp;
{
	curwin = wp;
}

wwhaschildren()
{
	register struct ww *wp;

	for (wp = wwhead; wp; wp = wp->ww_next)
		if (wp->ww_state == WW_HASPROC)
			return 1;
	return 0;
}
