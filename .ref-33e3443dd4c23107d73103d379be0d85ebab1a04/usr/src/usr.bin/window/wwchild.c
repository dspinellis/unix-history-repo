#ifndef lint
static char sccsid[] = "@(#)wwchild.c	3.5 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"
#include <sys/wait.h>

wwchild()
{
	extern errno;
	int olderrno;
	register struct ww *wp;
	union wait w;
	int pid;

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
}
