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
static char sccsid[] = "@(#)wwenviron.c	3.22 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <sys/signal.h>

/*
 * Set up the environment of this process to run in window 'wp'.
 */
wwenviron(wp)
register struct ww *wp;
{
	register i;
#ifndef TIOCSCTTY
	int pgrp = getpid();
#endif
	char buf[1024];

#ifndef TIOCSCTTY
	if ((i = open("/dev/tty", 0)) < 0)
		goto bad;
	if (ioctl(i, TIOCNOTTY, (char *)0) < 0)
		goto bad;
	(void) close(i);
#endif
	if ((i = wp->ww_socket) < 0) {
		if ((i = open(wp->ww_ttyname, 2)) < 0)
			goto bad;
		if (wwsettty(i, &wwwintty, (struct ww_tty *)0) < 0)
			goto bad;
	}
	(void) dup2(i, 0);
	(void) dup2(i, 1);
	(void) dup2(i, 2);
	for (i = wwdtablesize - 1; i > 2; i--)
		(void) close(i);
#ifdef TIOCSCTTY
	(void) setsid(0);
	(void) ioctl(0, TIOCSCTTY, 0);
#else
	(void) ioctl(0, TIOCSPGRP, (char *)&pgrp);
	(void) setpgrp(pgrp, pgrp);
#endif
	/* SIGPIPE is the only one we ignore */
	(void) signal(SIGPIPE, SIG_DFL);
	(void) sigsetmask(0);
	/*
	 * Two conditions that make destructive setenv ok:
	 * 1. setenv() copies the string,
	 * 2. we've already called tgetent which copies the termcap entry.
	 */
	(void) sprintf(buf, "%sco#%d:li#%d:%s",
		WWT_TERMCAP, wp->ww_w.nc, wp->ww_w.nr, wwwintermcap);
	(void) setenv("TERMCAP", buf, 1);
	(void) sprintf(buf, "%d", wp->ww_id + 1);
	(void) setenv("WINDOW_ID", buf, 1);
	return 0;
bad:
	wwerrno = WWE_SYS;
	return -1;
}
