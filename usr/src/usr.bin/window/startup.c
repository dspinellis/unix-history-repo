#ifndef lint
static	char *sccsid = "@(#)startup.c	3.5 83/08/26";
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

	if ((w = openwin(-1, 1, 0, r, wwncol, nbufline)) == 0)
		goto bad;
	if (openwin(-1, r + 2, 0, wwnrow - r - 2, wwncol, nbufline) == 0)
		goto bad;
	(void) wwprintf(w, "Escape character is %s.\r\n", unctrl(escapec));
	setselwin(w);
	return;
bad:
	(void) wwputs("\nCan't open default windows.  ", cmdwin);
}
