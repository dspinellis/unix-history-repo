/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)wwpty.c	3.14 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"

wwgetpty(w)
register struct ww *w;
{
	register char c, *p;
	int tty;
	int on = 1;
#define PTY "/dev/XtyXX"
#define _PT	5
#define _PQRS	8
#define _0_9	9

	(void) strcpy(w->ww_ttyname, PTY);
	for (c = 'p'; c <= 'u'; c++) {
		w->ww_ttyname[_PT] = 'p';
		w->ww_ttyname[_PQRS] = c;
		w->ww_ttyname[_0_9] = '0';
		if (access(w->ww_ttyname, 0) < 0)
			break;
		for (p = "0123456789abcdef"; *p; p++) {
			w->ww_ttyname[_PT] = 'p';
			w->ww_ttyname[_0_9] = *p;
			if ((w->ww_pty = open(w->ww_ttyname, 2)) < 0)
				continue;
			w->ww_ttyname[_PT] = 't';
			if ((tty = open(w->ww_ttyname, 2)) < 0) {
				(void) close(w->ww_pty);
				continue;
			}
			(void) close(tty);
			if (ioctl(w->ww_pty, TIOCPKT, (char *)&on) < 0) {
				(void) close(w->ww_pty);
				continue;
			}
			return 0;
		}
	}
	w->ww_pty = -1;
	wwerrno = WWE_NOPTY;
	return -1;
}
