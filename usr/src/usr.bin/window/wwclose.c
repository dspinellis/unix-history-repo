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
static char sccsid[] = "@(#)wwclose.c	3.16 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"

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
