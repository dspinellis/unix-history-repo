#ifndef lint
static	char *sccsid = "@(#)wwtty.c	2.1.1.1 83/08/09";
#endif

#include "ww.h"

wwgettty(d, t)
register struct ww_tty *t;
{
	if (ioctl(d, TIOCGETP, (char *)&t->ww_sgttyb) < 0)
		return -1;
	if (ioctl(d, TIOCGETC, (char *)&t->ww_tchars) < 0)
		return -1;
	if (ioctl(d, TIOCGLTC, (char *)&t->ww_ltchars) < 0)
		return -1;
	if (ioctl(d, TIOCLGET, (char *)&t->ww_lmode) < 0)
		return -1;
	if (ioctl(d, TIOCGETD, (char *)&t->ww_ldisc) < 0)
		return -1;
	if (ioctl(d, TIOCGPGRP, (char *)&t->ww_pgrp) < 0)
		return -1;
	return 0;
}

wwsettty(d, t)
register struct ww_tty *t;
{
	if (ioctl(d, TIOCSETP, (char *)&t->ww_sgttyb) < 0)
		return -1;
	if (ioctl(d, TIOCSETC, (char *)&t->ww_tchars) < 0)
		return -1;
	if (ioctl(d, TIOCSLTC, (char *)&t->ww_ltchars) < 0)
		return -1;
	if (ioctl(d, TIOCLSET, (char *)&t->ww_lmode) < 0)
		return -1;
	if (ioctl(d, TIOCSETD, (char *)&t->ww_ldisc) < 0)
		return -1;
	if (ioctl(d, TIOCSPGRP, (char *)&t->ww_pgrp) < 0)
		return -1;
	return 0;
}
