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
static char sccsid[] = "@(#)wwclose.c	3.14 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <signal.h>

wwclose(w)
register struct ww *w;
{
	register struct ww **p;

	for (p = &wwhead; *p && *p != w; p = &(*p)->ww_next)
		;
	if (*p == 0)
		return -1;
	*p = w->ww_next;
	if (curwin == w)
		curwin = wwhead;
	if (w->ww_state == WW_HASPROC)
		kill(w->ww_pid, SIGHUP);
	close(w->ww_tty);
	close(w->ww_pty);
	Wclose(w->ww_win);
	cfree(w);
	return 0;
}
