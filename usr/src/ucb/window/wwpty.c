#ifndef lint
static char sccsid[] = "@(#)wwpty.c	3.11 4/24/85";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"

/*
 * To satisfy Chris, we allocate pty's backwards, and if
 * there are more than the ptyp's (i.e. the ptyq's)
 * on the machine, we don't use the p's.
 */
wwgetpty(w)
register struct ww *w;
{
	register char c;
	register int i;
	int tty;
	int on = 1;
	char hasq = 0;
#define PTY "/dev/XtyXX"
#define _PT	5
#define _PQRS	8
#define _0_9	9

	(void) strcpy(w->ww_ttyname, PTY);
	for (c = 's'; c >= 'p'; c--) {
		w->ww_ttyname[_PT] = 'p';
		w->ww_ttyname[_PQRS] = c;
		w->ww_ttyname[_0_9] = '0';
		if (access(w->ww_ttyname, 0) < 0)
			continue;
		if (c != 'p')
			hasq = 1;
		else if (hasq)
			break;
		for (i = 15; i >= 0; i--) {
			w->ww_ttyname[_PT] = 'p';
			w->ww_ttyname[_0_9] = "0123456789abcdef"[i];
			if ((w->ww_pty = open(w->ww_ttyname, 2)) < 0)
				continue;
			w->ww_ttyname[_PT] = 't';
			if ((tty = open(w->ww_ttyname, 2)) < 0) {
				(void) close(w->ww_pty);
				continue;
			}
			(void) close(tty);
			if (ioctl(w->ww_pty, (int)TIOCPKT, (char *)&on) < 0) {
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
