#ifndef lint
static	char *sccsid = "@(#)wwclose.c	3.3 83/08/23";
#endif

#include "ww.h"
#include <signal.h>

wwclose(w)
register struct ww *w;
{
	wwindex[w->ww_index] = 0;
	if (w->ww_state == WWS_HASPROC)
		(void) kill(w->ww_pid, SIGHUP);
	(void) close(w->ww_tty);
	(void) close(w->ww_pty);
	wwfree((char **)w->ww_win);
	wwfree((char **)w->ww_cov);
	wwfree((char **)w->ww_buf);
	free((char *)w);
}
