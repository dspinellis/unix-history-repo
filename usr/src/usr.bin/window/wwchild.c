#ifndef lint
static	char *sccsid = "@(#)wwchild.c	3.2 83/09/01";
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
				break;
			}
		}
	}
}
