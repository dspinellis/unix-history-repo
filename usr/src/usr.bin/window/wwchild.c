#ifndef lint
static	char *sccsid = "@(#)wwchild.c	3.2 83/09/01";
#endif

#include "ww.h"
#include <sys/wait.h>

wwchild()
{
	register struct ww **wp;
	union wait w;
	int pid;

	while ((pid = wait3(&w, WNOHANG|WUNTRACED, (struct rusage *)0)) > 0) {
		for (wp = wwindex; wp < &wwindex[NWW]; wp++) {
			if (*wp && (*wp)->ww_state == WWS_HASPROC
			    && (*wp)->ww_pid == pid) {
				(*wp)->ww_state = WWS_DEAD;
				break;
			}
		}
	}
}
