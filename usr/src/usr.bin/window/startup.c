#ifndef lint
static	char *sccsid = "@(#)startup.c	3.2 83/08/17";
#endif

#include "defs.h"

doconfig()
{
	char buf[100];
	char *home;

	if ((home = getenv("HOME")) == 0)
		home = "";
	(void) sprintf(buf, "%s/.windowrc", home);
	return dosource(buf);
}

/*
 * The default is two windows of equal sizes.
 */
dodefault()
{
	struct ww *w;
	register r = wwnrow / 2 - 1;

	if ((w = openwin(-1, r, wwncol, 1, 0)) == 0)
		goto bad;
	if (openwin(-1, wwnrow - r - 2, wwncol, r + 2, 0) == 0)
		goto bad;
	wwprintf(w, "Escape character is %s.\r\n", unctrl(escapec));
	setselwin(w);
	return;
bad:
	(void) wwputs("Can't open default windows.  ", cmdwin);
}
