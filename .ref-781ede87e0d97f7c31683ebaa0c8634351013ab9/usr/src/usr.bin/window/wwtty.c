/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwtty.c	3.17 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <fcntl.h>
#if !defined(OLD_TTY) && !defined(TIOCGWINSZ)
#include <sys/ioctl.h>
#endif

wwgettty(d, t)
register struct ww_tty *t;
{
#ifdef OLD_TTY
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
#else
	if (tcgetattr(d, &t->ww_termios) < 0)
		goto bad;
#endif
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
wwsettty(d, t)
register struct ww_tty *t;
{
#ifdef OLD_TTY
	int i;

	/* XXX, for buggy tty drivers that don't wait for output to drain */
	while (ioctl(d, TIOCOUTQ, &i) >= 0 && i > 0)
		usleep(100000);
	if (ioctl(d, TIOCSETN, (char *)&t->ww_sgttyb) < 0)
		goto bad;
	if (ioctl(d, TIOCSETC, (char *)&t->ww_tchars) < 0)
		goto bad;
	if (ioctl(d, TIOCSLTC, (char *)&t->ww_ltchars) < 0)
		goto bad;
	if (ioctl(d, TIOCLSET, (char *)&t->ww_lmode) < 0)
		goto bad;
	if (ioctl(d, TIOCGETD, (char *)&i) < 0)
		goto bad;
	if (t->ww_ldisc != i &&
	    ioctl(d, TIOCSETD, (char *)&t->ww_ldisc) < 0)
		goto bad;
#else
#ifdef sun
	/* XXX, for buggy tty drivers that don't wait for output to drain */
	(void) tcdrain(d);
#endif
	if (tcsetattr(d, TCSADRAIN, &t->ww_termios) < 0)
		goto bad;
#endif
	if (fcntl(d, F_SETFL, t->ww_fflags) < 0)
		goto bad;
	return 0;
bad:
	wwerrno = WWE_SYS;
	return -1;
}

/*
 * The ttysize and stop-start routines must also work
 * on the control side of pseudoterminals.
 */

wwgetttysize(d, r, c)
	int *r, *c;
{
	struct winsize winsize;

	if (ioctl(d, TIOCGWINSZ, (char *)&winsize) < 0) {
		wwerrno = WWE_SYS;
		return -1;
	}
	if (winsize.ws_row != 0)
		*r = winsize.ws_row;
	if (winsize.ws_col != 0)
		*c = winsize.ws_col;
	return 0;
}

wwsetttysize(d, r, c)
{
	struct winsize winsize;

	winsize.ws_row = r;
	winsize.ws_col = c;
	winsize.ws_xpixel = winsize.ws_ypixel = 0;
	if (ioctl(d, TIOCSWINSZ, (char *)&winsize) < 0) {
		wwerrno = WWE_SYS;
		return -1;
	}
	return 0;
}

wwstoptty(d)
{
#if !defined(OLD_TTY) && defined(TCOOFF)
	/* not guaranteed to work on the pty side */
	if (tcflow(d, TCOOFF) < 0)
#else
	if (ioctl(d, TIOCSTOP, (char *)0) < 0)
#endif
	{
		wwerrno = WWE_SYS;
		return -1;
	}
	return 0;
}

wwstarttty(d)
{
#if !defined(OLD_TTY) && defined(TCOON)
	/* not guaranteed to work on the pty side */
	if (tcflow(d, TCOON) < 0)
#else
	if (ioctl(d, TIOCSTART, (char *)0) < 0)
#endif
	{
		wwerrno = WWE_SYS;
		return -1;
	}
	return 0;
}
