#ifndef lint
static	char *sccsid = "@(#)wwtty.c	3.6 84/03/03";
#endif

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

wwsettty(d, t)
register struct ww_tty *t;
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
