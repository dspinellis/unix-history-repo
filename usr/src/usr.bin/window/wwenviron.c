#ifndef lint
static	char *sccsid = "@(#)wwenviron.c	3.9 84/03/23";
#endif

#include "ww.h"

/*
 * Set up the environment of this process to run in window 'wp'.
 * Can't report errors in any intelligent way, because the parent
 * hangs in vfork() until we die, but we can't die until output
 * drains (i.e. deadlock).  So don't say anything.
 */
wwenviron(wp)
register struct ww *wp;
{
	register i;
	int pgrp = getpid();

	i = open("/dev/tty", 0);
	if (i < 0)
		return;
	if (ioctl(i, (int)TIOCNOTTY, (char *)0) < 0)
		return;
	(void) close(i);
	if ((i = open(wp->ww_ttyname, 2)) < 0)
		return;
	(void) dup2(i, 0);
	(void) dup2(i, 1);
	(void) dup2(i, 2);
	for (i = wwdtablesize - 1; i > 2; i--)
		(void) close(i);
	if (ioctl(0, (int)TIOCSPGRP, (char *)&pgrp) < 0)
		return;
	(void) setpgrp(pgrp, pgrp);

	(void) sprintf(wwwintermcap, "TERMCAP=%sco#%d:li#%d:%s%s%s%s",
		WWT_TERMCAP, wp->ww_w.nc, wp->ww_w.nr,
		wwavailmodes & WWM_REV ? WWT_REV : "",
		wwavailmodes & WWM_UL ? WWT_UL : "",
		wwavailmodes & WWM_GRP ? WWT_GRP : "",
		wwkeys);
}
