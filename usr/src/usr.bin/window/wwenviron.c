#ifndef lint
static	char *sccsid = "@(#)wwenviron.c	3.10 84/04/08";
#endif

#include "ww.h"

/*
 * Set up the environment of this process to run in window 'wp'.
 */
wwenviron(wp)
register struct ww *wp;
{
	register i;
	int pgrp = getpid();

	if ((i = open("/dev/tty", 0)) < 0)
		goto bad;
	if (ioctl(i, (int)TIOCNOTTY, (char *)0) < 0)
		goto bad;
	(void) close(i);
	if ((i = wp->ww_socket) < 0 && (i = open(wp->ww_ttyname, 2)) < 0)
		goto bad;
	(void) dup2(i, 0);
	(void) dup2(i, 1);
	(void) dup2(i, 2);
	for (i = wwdtablesize - 1; i > 2; i--)
		(void) close(i);
	(void) ioctl(0, (int)TIOCSPGRP, (char *)&pgrp);
	(void) setpgrp(pgrp, pgrp);

	(void) sprintf(wwwintermcap, "TERMCAP=%sco#%d:li#%d:%s%s%s%s",
		WWT_TERMCAP, wp->ww_w.nc, wp->ww_w.nr,
		wwavailmodes & WWM_REV ? WWT_REV : "",
		wwavailmodes & WWM_UL ? WWT_UL : "",
		wwavailmodes & WWM_GRP ? WWT_GRP : "",
		wwkeys);
	return 0;
bad:
	wwerrno = WWE_SYS;
	return -1;
}
