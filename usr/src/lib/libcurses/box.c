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
static char sccsid[] = "@(#)box.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"curses.ext"

/*
 *	This routine draws a box around the given window with "vert"
 * as the vertical delimiting char, and "hor", as the horizontal one.
 *
 */
box(win, vert, hor)
reg WINDOW	*win;
char		vert, hor; {

	reg int		i;
	reg int		endy, endx;
	reg char	*fp, *lp;

	endx = win->_maxx;
	endy = win->_maxy - 1;
	fp = win->_y[0];
	lp = win->_y[endy];
	for (i = 0; i < endx; i++)
		fp[i] = lp[i] = hor;
	endx--;
	for (i = 0; i <= endy; i++)
		win->_y[i][0] = (win->_y[i][endx] = vert);
	if (!win->_scroll && (win->_flags&_SCROLLWIN))
		fp[0] = fp[endx] = lp[0] = lp[endx] = ' ';
	touchwin(win);
}
