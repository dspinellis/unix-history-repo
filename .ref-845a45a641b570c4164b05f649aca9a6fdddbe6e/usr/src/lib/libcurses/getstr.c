/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getstr.c	8.1 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * wgetstr --
 *	Get a string starting at (cury, curx).
 */
int
wgetstr(win, str)
	register WINDOW *win;
	register char *str;
{
	while ((*str = wgetch(win)) != ERR && *str != '\n')
		str++;
	if (*str == ERR) {
		*str = '\0';
		return (ERR);
	}
	*str = '\0';
	return (OK);
}
