#ifndef lint
static	char *sccsid = "@(#)wwclose.c	3.1 83/08/11";
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
	wwfree((char **)w->ww_win, w->ww_w.nr);
	wwfree((char **)w->ww_cov, w->ww_w.nr);
	wwfree((char **)w->ww_buf, w->ww_nline);
	free((char *)w);
}
