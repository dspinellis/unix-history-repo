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
static char sccsid[] = "@(#)wwtty.c	3.13 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <fcntl.h>

wwgettty(d, t)
register struct ww_tty *t;
{
	if (ioctl(d, TIOCGETP, &t->ww_sgttyb) < 0)
		goto bad;
	if (ioctl(d, TIOCGETC, &t->ww_tchars) < 0)
		goto bad;
	if (ioctl(d, TIOCGLTC, &t->ww_ltchars) < 0)
		goto bad;
	if (ioctl(d, TIOCLGET, &t->ww_lmode) < 0)
		goto bad;
	if (ioctl(d, TIOCGETD, &t->ww_ldisc) < 0)
		goto bad;
	if ((t->ww_fflags = fcntl(d, F_GETFL, 0)) < 0)
		goto bad;
	if (ioctl(d, TIOCGPGRP, &t->ww_pgrp) < 0)
	return 0;
bad:
	wwerrno = WWE_SYS;
	return -1;
}

/*
 * Set the modes of tty 'd' to 't'
 * 'o' is the current modes.  We set the line discipline only if
 * it changes, to avoid unnecessary flushing of typeahead.
 */
wwsettty(d, t, o)
register struct ww_tty *t, *o;
{
	if (ioctl(d, TIOCSETP, &t->ww_sgttyb) < 0)
		goto bad;
	if (ioctl(d, TIOCSETC, &t->ww_tchars) < 0)
		goto bad;
	if (ioctl(d, TIOCSLTC, &t->ww_ltchars) < 0)
		goto bad;
	if (ioctl(d, TIOCLSET, &t->ww_lmode) < 0)
		goto bad;
	if (ioctl(d, TIOCSETD, &t->ww_ldisc) < 0)
		goto bad;
	return 0;
bad:
	wwerrno = WWE_SYS;
	return -1;
}
