/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)addch.c	5.5 (Berkeley) %G%";
#endif /* not lint */

# include	"curses.ext"

/*
 *	This routine adds the character to the current position
 *
 */
waddch(win, c)
WINDOW	*win;
char		c;
{
    return waddbytes(win, &c, 1);
}
