#ifndef lint
static	char *sccsid = "@(#)startup.c	3.5 83/08/26";
#endif

#include "defs.h"

struct ww *doopen();
char *getenv();

doconfig()
{
	char buf[100];
	char *home;

	if ((home = getenv("HOME")) == 0)
		home = "";
	sprintf(buf, "%s/.windowrc", home);
	return dosource(buf);
}

/*
 * The default is two windows of equal sizes.
 */
dodefault()
{
	struct ww *w;
	register r = wwnrow / 2 - 1;

	if ((w = doopen(-1, r + 1, wwncol, 0, 0)) == 0)
		goto bad;
	if (doopen(-1, wwnrow - r, wwncol, r, 0) == 0)
		goto bad;
	(void) wwprintf(w, "Escape character is %s.\r\n", unctrl(escapec));
	setselwin(w);
	return;
bad:
	wwputs("Can't open default windows.  ", cmdwin);
}
