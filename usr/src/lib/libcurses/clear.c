/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)clear.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"curses.ext"

/*
 *	This routine clears the window.
 *
 */
wclear(win)
reg WINDOW	*win; {

	werase(win);
	win->_clear = TRUE;
	return OK;
}
