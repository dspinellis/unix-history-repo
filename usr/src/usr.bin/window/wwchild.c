#ifndef lint
static	char *sccsid = "@(#)wwchild.c	3.3 83/12/01";
#endif

#include "ww.h"
#include <sys/wait.h>

wwchild()
{
	extern errno;
	int olderrno;
	register struct ww **wp;
	union wait w;
	int pid;

	olderrno = errno;
	while ((pid = wait3(&w, WNOHANG|WUNTRACED, (struct rusage *)0)) > 0) {
		for (wp = wwindex; wp < &wwindex[NWW]; wp++) {
			if (*wp && (*wp)->ww_state == WWS_HASPROC
			    && (*wp)->ww_pid == pid) {
				(*wp)->ww_state = WWS_DEAD;
				break;
			}
		}
	}
	errno = olderrno;
}
