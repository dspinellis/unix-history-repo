#ifndef lint
static	char *sccsid = "@(#)wwspawn.c	3.3 83/08/26";
#endif

#include "ww.h"

extern int _wwdtablesize;
extern char _wwtermcap[];

wwfork(wp)
register struct ww *wp;
{
	switch (wp->ww_pid = fork()) {
	case -1:
		wwerrno = WWE_SYS;
		return -1;
	case 0:
		moncontrol(0);
		wp->ww_state = WW_INCHILD;
		wwenviron(wp);
		return 0;
	default:
		wp->ww_state = WW_HASPROC;
		close(wp->ww_tty);
		wp->ww_tty = -1;
		return wp->ww_pid;
	}
}
