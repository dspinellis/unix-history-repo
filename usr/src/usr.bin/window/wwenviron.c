#ifndef lint
static	char *sccsid = "@(#)wwenviron.c	3.8 84/01/13";
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
	static char **termcap = 0;
	static char *tbuf;
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

	/*
	 * Do this only once if vfork().
	 */
	if (termcap == 0) {
		extern char **environ;
		static char **env;
		register char **p, **q;

		for (i = 0, p = environ; *p; p++, i++)
			;
		env = (char **)malloc((unsigned)(i + 3) * sizeof (char *));
		if (env == 0)
			return;
		if ((tbuf = malloc((unsigned) 1024)) == 0)
			return;
		for (p = environ, q = env; *p; p++, q++) {
			if (strncmp(*p, "TERM=", 5) == 0)
				*q = WWT_TERM;
			else if (strncmp(*p, "TERMCAP=", 8) == 0)
				termcap = q;
			else
				*q = *p;
		}
		if (termcap == 0)
			termcap = q++;
		*q = 0;
		environ = env;
	}
	*termcap = sprintf(tbuf, "TERMCAP=%sco#%d:li#%d:%s%s%s%s",
		WWT_TERMCAP, wp->ww_w.nc, wp->ww_w.nr,
		wwavailmodes & WWM_REV ? WWT_REV : "",
		wwavailmodes & WWM_UL ? WWT_UL : "",
		wwavailmodes & WWM_GRP ? WWT_GRP : "",
		wwkeys);
}
