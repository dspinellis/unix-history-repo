#ifndef lint
static	char *sccsid = "@(#)wwenviron.c	3.2 83/08/18";
#endif

#include "ww.h"

extern char **environ;

wwenviron(wp)
register struct ww *wp;
{
	register i;
	register char **p, **q;
	char **termcap = 0;
	char **env;
	char *tbuf;

	(void) dup2(wp->ww_tty, 0);
	(void) dup2(wp->ww_tty, 1);
	(void) dup2(wp->ww_tty, 2);
	for (i = wwdtablesize - 1; i > 2; i--)
		(void) close(i);

	i = open("/dev/tty");
	(void) ioctl(i, (int)TIOCNOTTY, (char *)0);
	(void) close(i);
	(void) open(wp->ww_ttyname, 0);

	for (i = 0, p = environ; *p; p++, i++)
		;
	if ((env = (char **)malloc((unsigned)(i + 3) * sizeof (char *))) == 0)
		return;			/* can't report error */
	if ((tbuf = malloc((unsigned) 1024)) == 0)
		return;			/* can't report error */
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
	*termcap = sprintf(tbuf, "TERMCAP=%sco#%d:li#%d:",
		WWT_TERMCAP, wp->ww_w.nc, wp->ww_w.nr);
	if (wwavailmodes & WWM_REV)
		(void) strcat(tbuf, WWT_REV);
	if (wwavailmodes & WWM_UL)
		(void) strcat(tbuf, WWT_UL);
	(void) strcat(tbuf, wwkeys);
	environ = env;
}
