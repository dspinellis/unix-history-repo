#ifndef lint
static	char *sccsid = "@(#)wwspawn.c	2.1.1.1 83/08/09";
#endif

#include "ww.h"

wwfork(wp)
register struct ww *wp;
{
	switch (wp->ww_pid = fork()) {
	case -1:
		return -1;
	case 0:
		wp->ww_state = WWS_INCHILD;
		wwenviron(wp);
		return 0;
	default:
		wp->ww_state = WWS_HASPROC;
		(void) close(wp->ww_tty);
		wp->ww_tty = -1;
		return wp->ww_pid;
	}
}

#define TERM	"TERM=window"
#define TERMCAP	"TERMCAP=WW|window|window package:\
	:cr=^M:nl=^J:bl=^G:\
	:al=\\EL:am:le=^H:bs:cd=\\EJ:ce=\\EK:cl=\\EE:cm=\\EY%%+ %%+ :\
	:co#%d:dc=\\EN:dl=\\EM:do=\\EB:ei=\\EO:ho=\\EH:li#%d:im=\\E@:mi:\
	:nd=\\EC:ta=^I:pt:up=\\EA:"
static char *env[100];
static char buf[1024];
extern char **environ;

wwenviron(wp)
register struct ww *wp;
{
	register i;
	register char **p, **q;
	char **termcap = 0;

	(void) dup2(wp->ww_tty, 0);
	(void) dup2(wp->ww_tty, 1);
	(void) dup2(wp->ww_tty, 2);
	for (i = wwdtablesize - 1; i > 2; i--)
		(void) close(i);

	i = open("/dev/tty");
	(void) ioctl(i, (int)TIOCNOTTY, (char *)0);
	(void) close(i);
	(void) open(wp->ww_ttyname, 0);

	for (p = environ, q = env; *p; p++, q++) {
		if (strncmp(*p, "TERM=", 5) == 0)
			*q = TERM;
		else if (strncmp(*p, "TERMCAP=", 8) == 0)
			termcap = q;
		else
			*q = *p;
	}
	if (termcap == 0)
		termcap = q++;
	*q = 0;
	*termcap = sprintf(buf, TERMCAP, wp->ww_w.nc, wp->ww_w.nr);
	(void) strcat(buf, wwkeys);
	environ = env;
}
