#ifndef lint
static	char *sccsid = "@(#)wwmisc.c	3.2 83/09/02";
#endif

#include "ww.h"

/*
 * Sufficient but necessary test for total visibility.
 */
wwvisible(w)
register struct ww *w;
{
	register i;
	register nvis = 0;

	for (i = 0; i < w->ww_w.nr; i++)
		nvis += w->ww_nvis[i];
	if (w->ww_hascursor && wwsmap[wwcurrow(w)][wwcurcol(w)] == w->ww_index)
		nvis++;
	return nvis == w->ww_w.nr * w->ww_w.nc;
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
