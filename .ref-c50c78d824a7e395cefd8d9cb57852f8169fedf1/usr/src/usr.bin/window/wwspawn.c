/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwspawn.c	3.13 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <sys/signal.h>

/*
 * There is a dead lock with vfork and closing of pseudo-ports.
 * So we have to be sneaky about error reporting.
 */
wwspawn(wp, file, argv)
register struct ww *wp;
char *file;
char **argv;
{
	int pid;
	int ret;
	char erred = 0;
	int s;

	s = sigblock(sigmask(SIGCHLD));
	switch (pid = vfork()) {
	case -1:
		wwerrno = WWE_SYS;
		ret = -1;
		break;
	case 0:
		if (wwenviron(wp) >= 0)
			execvp(file, argv);
		erred = 1;
		_exit(1);
	default:
		if (erred) {
			wwerrno = WWE_SYS;
			ret = -1;
		} else {
			wp->ww_pid = pid;
			wp->ww_state = WWS_HASPROC;
			ret = pid;
		}
	}
	(void) sigsetmask(s);
	if (wp->ww_socket >= 0) {
		(void) close(wp->ww_socket);
		wp->ww_socket = -1;
	}
	return ret;
}
