#ifndef lint
static	char *sccsid = "@(#)wwspawn.c	3.2 83/08/17";
#endif

#include "ww.h"

wwfork(wp)
register struct ww *wp;
{
	switch (wp->ww_pid = fork()) {
	case -1:
		return -1;
	case 0:
		moncontrol(0);
		wp->ww_state = WWS_INCHILD;
		wwenviron(wp);
		return 0;
	default:
		wp->ww_state = WWS_HASPROC;
		(void) close(wp->ww_tty);
		wp->ww_tty = -1;
		return wp->ww_pid;
	}
}
