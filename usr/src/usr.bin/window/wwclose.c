#ifndef lint
static	char *sccsid = "@(#)wwclose.c	1.1 83/07/18";
#endif

#include "ww.h"
#include <signal.h>

wwclose(w)
register struct ww *w;
{
	register struct ww **p;

	for (p = &wwhead; *p && *p != w; p = &(*p)->ww_next)
		;
	if (*p == 0)
		return -1;
	*p = w->ww_next;
	if (curwin == w)
		curwin = wwhead;
	if (w->ww_state == WW_HASPROC)
		kill(w->ww_pid, SIGHUP);
	close(w->ww_tty);
	close(w->ww_pty);
	Wclose(w->ww_win);
	cfree(w);
	return 0;
}
