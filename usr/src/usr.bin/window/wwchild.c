#ifndef lint
static	char *sccsid = "@(#)wwchild.c	1.1 83/07/12";
#endif

#include "ww.h"
#include <sys/wait.h>

wwchild()
{
	register struct ww *wp;
	union wait w;
	int pid;
	/*
	char buf[100];
	*/

	while ((pid = wait3(&w, WNOHANG|WUNTRACED, 0)) > 0) {
		for (wp = _wwhead; wp; wp = wp->ww_next) {
			if (wp->ww_pid == pid) {
				wp->ww_state = WW_DEAD;
				/*
				(void) sprintf(buf, "\r\n%d: Died\r\n", pid);
				wwputstr(buf);
				*/
				break;
			}
		}
		/*
		if (wp == 0) {
			(void) sprintf(buf, "\r\n%d: No such child\r\n", pid);
			wwputstr(buf);
		}
		*/
	}
}
