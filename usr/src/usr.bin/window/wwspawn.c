#ifndef lint
static	char *sccsid = "@(#)wwspawn.c	3.6 83/12/01";
#endif

#include "ww.h"
#include <signal.h>

/*
 * There is a dead lock with vfork and closing of pseudo-ports.
 * So we have to be sneaky.
 */
/*VARARGS3*/
wwspawn(wp, file, argv0)
register struct ww *wp;
char *file, *argv0;
{
	extern int errno;
	extern char *sys_errlist[];
	int pid;

	sighold(SIGCHLD);
	switch (pid = vfork()) {
	case -1:
		wwerrno = WWE_SYS;
		sigrelse(SIGCHLD);
		return -1;
	case 0:
		wwenviron(wp);
		errno = 0;
		execv(file, &argv0);
		_exit(1);
	default:
		if (errno != 0) {
			wwerrno = WWE_SYS;
			sigrelse(SIGCHLD);
			return -1;
		} else {
			wp->ww_pid = pid;
			wp->ww_state = WWS_HASPROC;
			sigrelse(SIGCHLD);
			return pid;
		}
	}
}
