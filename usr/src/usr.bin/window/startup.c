#ifndef lint
static char sccsid[] = "@(#)startup.c	3.14 %G%";
#endif

#include "defs.h"
#include "value.h"
#include "var.h"
#include "char.h"
#include "local.h"

struct ww *doopen();
char *getenv();

doconfig()
{
	char buf[100];
	char *home;
	static char runcom[] = RUNCOM;

	if ((home = getenv("HOME")) == 0)
		home = ".";
	return dosource(sprintf(buf, "%.*s/%s",
		(sizeof buf - sizeof runcom) / sizeof (char) - 1,
		home, runcom));
}

/*
 * The default is two windows of equal size.
 */
dodefault()
{
	struct ww *w;
	register r = wwnrow / 2 - 1;

	if ((w = doopen(-1, r + 1, wwncol, 0, 0)) == 0)
		return;
	if (doopen(-1, wwnrow - r, wwncol, r, 0) == 0)
		return;
	wwprintf(w, "Escape character is %s.\r\n", unctrl(escapec));
	setselwin(w);
}
