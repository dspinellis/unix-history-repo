#ifndef lint
static	char *sccsid = "@(#)wwenviron.c	3.4 83/11/29";
#endif

#include "ww.h"

/*
 * Set up the environment of this process to run in window 'wp'.
 * Can't report errors in any intelligent way, so don't.
 */
wwenviron(wp)
register struct ww *wp;
{
	static char **termcap = 0;
	static char *tbuf;
	register i;

	(void) dup2(wp->ww_tty, 0);
	(void) dup2(wp->ww_tty, 1);
	(void) dup2(wp->ww_tty, 2);
	for (i = wwdtablesize - 1; i > 2; i--)
		(void) close(i);

	i = open("/dev/tty", 0);
	if (i < 0) {
		perror("/dev/tty");
		return;
	}
	if (ioctl(i, (int)TIOCNOTTY, (char *)0) < 0) {
		perror("ioctl(TIOCNOTTY)");
		return;
	}
	(void) close(i);
	if (open(wp->ww_ttyname, 0) < 0) {
		perror(wp->ww_ttyname);
		return;
	}

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
	*termcap = sprintf(tbuf, "TERMCAP=%sco#%d:li#%d:",
		WWT_TERMCAP, wp->ww_w.nc, wp->ww_w.nr);
	if (wwavailmodes & WWM_REV)
		(void) strcat(tbuf, WWT_REV);
	if (wwavailmodes & WWM_UL)
		(void) strcat(tbuf, WWT_UL);
	(void) strcat(tbuf, wwkeys);
}
