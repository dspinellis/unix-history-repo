/*
 * Copyright (c) 1981, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)standout.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include "curses.h"

/*
 * wstandout
 *	Enter standout mode.
 */
int
wstandout(win)
	WINDOW *win;
{
	/*
	 * If standout/standend strings, or can underline, set the
	 * screen standout bit.
	 */
	if (SO != NULL && SE != NULL || UC != NULL)
		win->flags |= __WSTANDOUT;
	return (1);
}

/*
 * wstandend --
 *	Exit standout mode.
 */
int
wstandend(win)
	WINDOW *win;
{
	win->flags &= ~__WSTANDOUT;
	return (1);
}
