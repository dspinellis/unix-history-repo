/*
 * Copyright (c) 1980-1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)addch.c	5.2 (Berkeley) %G%";
#endif not lint

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
