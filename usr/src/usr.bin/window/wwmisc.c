#ifndef lint
static	char *sccsid = "@(#)wwmisc.c	1.1 83/07/12";
#endif

#include "ww.h"

struct ww *_wwcurrent = 0;

wwsetcurrent(wp)
register struct ww *wp;
{
	_wwcurrent = wp;
}

wwhaschildren()
{
	register struct ww *wp;

	for (wp = _wwhead; wp; wp = wp->ww_next)
		if (wp->ww_state == WW_HASPROC)
			return 1;
	return 0;
}
