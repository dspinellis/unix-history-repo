#ifndef lint
static char sccsid[] = "@(#)wwtty.c	3.10 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"
#include <fcntl.h>

wwgettty(d, t)
register struct ww_tty *t;
{
	if (ioctl(d, TIOCGETP, (char *)&t->ww_sgttyb) < 0)
		goto bad;
	if (ioctl(d, TIOCGETC, (char *)&t->ww_tchars) < 0)
		goto bad;
	if (ioctl(d, TIOCGLTC, (char *)&t->ww_ltchars) < 0)
		goto bad;
	if (ioctl(d, TIOCLGET, (char *)&t->ww_lmode) < 0)
		goto bad;
	if (ioctl(d, TIOCGETD, (char *)&t->ww_ldisc) < 0)
		goto bad;
	if ((t->ww_fflags = fcntl(d, F_GETFL, 0)) < 0)
		goto bad;
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
	if (ioctl(d, TIOCSETN, (char *)&t->ww_sgttyb) < 0)
		goto bad;
	if (ioctl(d, TIOCSETC, (char *)&t->ww_tchars) < 0)
		goto bad;
	if (ioctl(d, TIOCSLTC, (char *)&t->ww_ltchars) < 0)
		goto bad;
	if (ioctl(d, TIOCLSET, (char *)&t->ww_lmode) < 0)
		goto bad;
	if ((o == 0 || t->ww_ldisc != o->ww_ldisc) &&
	    ioctl(d, TIOCSETD, (char *)&t->ww_ldisc) < 0)
		goto bad;
	if (fcntl(d, F_SETFL, t->ww_fflags) < 0)
		goto bad;
	return 0;
bad:
	wwerrno = WWE_SYS;
	return -1;
}
