/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)startup.c	3.25 (Berkeley) %G%";
#endif /* not lint */

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
	(void) sprintf(buf, "%.*s/%s",
		(sizeof buf - sizeof runcom) / sizeof (char) - 1,
		home, runcom);
	return dosource(buf);
}

/*
 * The default is two windows of equal size.
 */
dodefault()
{
	struct ww *w;
	register r = wwnrow / 2 - 1;

	if ((w = doopen(-1, r + 1, wwncol, 0, 0)) == 0)
	if (doopen(-1, wwnrow - r, wwncol, r, 0) == 0)
		return;
	if ((w = openwin(0, 1, 0, r, wwncol, default_nline,
		(char *) 0, 1, 1, default_shellfile, default_shell)) == 0)
		return;
	wwprintf(w, "Escape character is %s.\r\n", unctrl(escapec));
}
