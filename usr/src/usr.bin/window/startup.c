#ifndef lint
static	char *sccsid = "@(#)startup.c	2.1.1.1 83/08/09";
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
	register r = wwnrow / 2;

	if ((w = openwin(-1, r + 1, wwncol, 0, 0)) == 0)
		goto bad;
	if (openwin(-1, wwnrow - r, wwncol, r, 0) == 0)
		goto bad;
	setselwin(w);
	return;
bad:
	(void) wwputs("Can't open default windows.  ", cmdwin);
}
