/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)overlay.c	5.6 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"curses.ext"
# include	<ctype.h>

# define	min(a,b)	(a < b ? a : b)
# define	max(a,b)	(a > b ? a : b)

/*
 *	This routine writes win1 on win2 non-destructively.
 *
 */
overlay(win1, win2)
reg WINDOW	*win1, *win2; {

	reg char	*sp, *end;
	reg int		x, y, endy, endx, starty, startx;
	reg int 	y1,y2;

# ifdef DEBUG
	fprintf(outf, "OVERLAY(%0.2o, %0.2o);\n", win1, win2);
# endif
	starty = max(win1->_begy, win2->_begy);
	startx = max(win1->_begx, win2->_begx);
	endy = min(win1->_maxy + win1->_begy, win2->_maxy + win2->_begx);
	endx = min(win1->_maxx + win1->_begx, win2->_maxx + win2->_begx);
# ifdef DEBUG
	fprintf(outf, "OVERLAY:from (%d,%d) to (%d,%d)\n", starty, startx, endy, endx);
# endif
	if (starty >= endy || startx >= endx)
		return;
	y1 = starty - win1->_begy;
	y2 = starty - win2->_begy;
	for (y = starty; y < endy; y++, y1++, y2++) {
		end = &win1->_y[y1][endx - win1->_begx];
		x = startx - win2->_begx;
		for (sp = &win1->_y[y1][startx - win1->_begx]; sp < end; sp++) {
			if (!isspace(*sp))
				mvwaddch(win2, y2, x, *sp);
			x++;
		}
	}
}
