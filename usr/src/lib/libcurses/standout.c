/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)standout.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * routines dealing with entering and exiting standout mode
 *
 */

# include	"curses.ext"

/*
 * enter standout mode
 */
char *
wstandout(win)
reg WINDOW	*win;
{
	if (!SO && !UC)
		return FALSE;

	win->_flags |= _STANDOUT;
	return (SO ? SO : UC);
}

/*
 * exit standout mode
 */
char *
wstandend(win)
reg WINDOW	*win;
{
	if (!SO && !UC)
		return FALSE;

	win->_flags &= ~_STANDOUT;
	return (SE ? SE : UC);
}
