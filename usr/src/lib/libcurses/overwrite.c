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
static char sccsid[] = "@(#)overwrite.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"curses.ext"
# include	<ctype.h>

# define	min(a,b)	(a < b ? a : b)
# define	max(a,b)	(a > b ? a : b)

/*
 *	This routine writes win1 on win2 destructively.
 *
 */
overwrite(win1, win2)
reg WINDOW	*win1, *win2; {

	reg char	*sp, *end;
	reg int		x, y, endy, endx, starty, startx;

# ifdef DEBUG
	fprintf(outf, "OVERWRITE(%0.2o, %0.2o);\n", win1, win2);
# endif
	starty = max(win1->_begy, win2->_begy);
	startx = max(win1->_begx, win2->_begx);
	endy = min(win1->_maxy + win1->_begy, win2->_maxy + win2->_begx);
	endx = min(win1->_maxx + win1->_begx, win2->_maxx + win2->_begx);
	if (starty >= endy || startx >= endx)
		return;
# ifdef DEBUG
	fprintf(outf, "OVERWRITE:from (%d,%d) to (%d,%d)\n", starty, startx, endy, endx);
# endif
	x = endx - startx;
	for (y = starty; y < endy; y++) {
		bcopy(&win1->_y[y - win1->_begy][startx - win1->_begx],
		      &win2->_y[y - win2->_begy][startx - win2->_begx], x);
		touchline(win2, y, startx - win2->_begx, endx - win2->_begx);
	}
}
