/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)standout.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>

/*
 * wstandout
 *	Enter standout mode.
 */
char *
wstandout(win)
	register WINDOW *win;
{
	if (!SO && !UC)
		return (0);

	win->flags |= __WSTANDOUT;
	return (SO ? SO : UC);
}

/*
 * wstandend --
 *	Exit standout mode.
 */
char *
wstandend(win)
	register WINDOW *win;
{
	if (!SO && !UC)
		return (0);

	win->flags &= ~__WSTANDOUT;
	return (SE ? SE : UC);
}
