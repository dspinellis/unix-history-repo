/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwspawn.c	3.15 (Berkeley) 6/6/90";
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
