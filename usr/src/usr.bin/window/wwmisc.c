#ifndef lint
static	char *sccsid = "@(#)wwmisc.c	2.1 83/07/30";
#endif

#include "ww.h"

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
