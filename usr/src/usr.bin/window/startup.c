#ifndef lint
static	char *sccsid = "@(#)startup.c	3.8 84/01/11";
#endif

#include "defs.h"
#include "value.h"
#include "var.h"

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
