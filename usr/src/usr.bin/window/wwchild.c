#ifndef lint
static	char *sccsid = "@(#)wwchild.c	3.1 83/08/11";
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
				/*
				(void) wwprintf(curwin,
					"\r\n%d: Died\r\n", pid);
				*/
				break;
			}
		}
		/*
		if (wp >= &wwindex[NWW])
			(void) wwprintf(curwin, "\r\n%d: No such child\r\n", pid);
		*/
	}
}
