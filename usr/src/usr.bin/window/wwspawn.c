#ifndef lint
static	char *sccsid = "@(#)wwspawn.c	1.1 83/07/12";
#endif

#include "ww.h"

wwfork(wp)
register struct ww *wp;
{
	switch (wp->ww_pid = fork()) {
	case -1:
		return -1;
	case 0:
		wp->ww_state = WW_INCHILD;
		return 0;
	default:
		wp->ww_state = WW_HASPROC;
		return wp->ww_pid;
	}
}
