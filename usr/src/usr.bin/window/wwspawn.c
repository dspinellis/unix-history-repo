#ifndef lint
static	char *sccsid = "@(#)wwspawn.c	3.5 83/11/29";
#endif

#include "ww.h"

/*VARARGS3*/
wwspawn(wp, file, argv0)
register struct ww *wp;
char *file, *argv0;
{
	int pid;

	switch (pid = vfork()) {
	case -1:
		wwerrno = WWE_SYS;
		return -1;
	case 0:
		wwenviron(wp);
		execv(file, &argv0);
		perror(file);
		exit(1);
	default:
		wp->ww_pid = pid;
		wp->ww_state = WWS_HASPROC;
		(void) close(wp->ww_tty);
		wp->ww_tty = -1;
		return pid;
	}
}
