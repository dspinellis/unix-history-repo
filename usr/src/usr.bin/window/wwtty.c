#ifndef lint
static	char *sccsid = "@(#)wwtty.c	1.2 83/07/18";
#endif

#include "ww.h"

wwgettty(d, t)
register struct ww_tty *t;
{
	if (ioctl(d, TIOCGETP, &t->ww_sgttyb) < 0)
		return -1;
	if (ioctl(d, TIOCGETC, &t->ww_tchars) < 0)
		return -1;
	if (ioctl(d, TIOCGLTC, &t->ww_ltchars) < 0)
		return -1;
	if (ioctl(d, TIOCLGET, &t->ww_lmode) < 0)
		return -1;
	if (ioctl(d, TIOCGETD, &t->ww_ldisc) < 0)
		return -1;
	if (ioctl(d, TIOCGPGRP, &t->ww_pgrp) < 0)
		return -1;
	return 0;
}

wwsettty(d, t)
register struct ww_tty *t;
{
	if (ioctl(d, TIOCSETP, &t->ww_sgttyb) < 0)
		return -1;
	if (ioctl(d, TIOCSETC, &t->ww_tchars) < 0)
		return -1;
	if (ioctl(d, TIOCSLTC, &t->ww_ltchars) < 0)
		return -1;
	if (ioctl(d, TIOCLSET, &t->ww_lmode) < 0)
		return -1;
	if (ioctl(d, TIOCSETD, &t->ww_ldisc) < 0)
		return -1;
	if (ioctl(d, TIOCSPGRP, &t->ww_pgrp) < 0)
		return -1;
	return 0;
}
