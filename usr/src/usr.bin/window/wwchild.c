/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwchild.c	3.11 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <sys/wait.h>

wwchild()
{
	extern errno;
	int olderrno;
	register struct ww *wp;
	union wait w;
	int pid;
	char collected = 0;

	olderrno = errno;
	while ((pid = wait3(&w, WNOHANG|WUNTRACED, 0)) > 0) {
		for (wp = wwhead; wp; wp = wp->ww_next) {
			if (wp->ww_pid == pid) {
				wp->ww_state = WW_DEAD;
				break;
			}
		}
	}
	errno = olderrno;
	/* jump out of wwiomux when somebody dies */
	if (collected)
		wwsetintr();
}
