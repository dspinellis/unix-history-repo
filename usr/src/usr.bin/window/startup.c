#ifndef lint
static	char *sccsid = "@(#)startup.c	1.3 83/07/29";
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
	register r = wwnrow / 2;

	if ((w = doopen(-1, r + 1, wwncol, 0, 0)) == 0)
		goto bad;
	if (doopen(-1, wwnrow - r, wwncol, r, 0) == 0)
		goto bad;
	setselwin(w);
	return;
bad:
	wwputs("Can't open default windows.  ", cmdwin);
}
