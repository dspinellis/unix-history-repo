/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)standout.c	5.5 (Berkeley) %G%";
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

	win->_flags |= _STANDOUT;
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

	win->_flags &= ~_STANDOUT;
	return (SE ? SE : UC);
}
