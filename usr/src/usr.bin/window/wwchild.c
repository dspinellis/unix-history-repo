#ifndef lint
static	char *sccsid = "@(#)wwchild.c	2.1 83/07/30";
#endif

#include "ww.h"
#include <sys/wait.h>

wwchild()
{
	register struct ww *wp;
	union wait w;
	int pid;

	while ((pid = wait3(&w, WNOHANG|WUNTRACED, 0)) > 0) {
		for (wp = wwhead; wp; wp = wp->ww_next) {
			if (wp->ww_pid == pid) {
				wp->ww_state = WW_DEAD;
				/*
				wwprintf(curwin,
					"\r\n%d: Died\r\n", pid);
				*/
				break;
			}
		}
		/*
		if (wp == 0)
			wwprintf(curwin, "\r\n%d: No such child\r\n", pid);
		*/
	}
}
