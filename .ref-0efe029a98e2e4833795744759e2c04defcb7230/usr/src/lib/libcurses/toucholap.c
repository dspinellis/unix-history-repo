/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)toucholap.c	5.4 (Berkeley) %G%";
#endif /* not lint */

# include	"curses.ext"

# define	min(a,b)	(a < b ? a : b)
# define	max(a,b)	(a > b ? a : b)

/*
 *	Touch, on win2, the part that overlaps with win1.
 *
 */
touchoverlap(win1, win2)
reg WINDOW	*win1, *win2; {

	reg int		x, y, endy, endx, starty, startx;

# ifdef DEBUG
	fprintf(outf, "TOUCHOVERLAP(%0.2o, %0.2o);\n", win1, win2);
# endif
	starty = max(win1->_begy, win2->_begy);
	startx = max(win1->_begx, win2->_begx);
	endy = min(win1->_maxy + win1->_begy, win2->_maxy + win2->_begx);
	endx = min(win1->_maxx + win1->_begx, win2->_maxx + win2->_begx);
# ifdef DEBUG
	fprintf(outf, "TOUCHOVERLAP:from (%d,%d) to (%d,%d)\n", starty, startx, endy, endx);
	fprintf(outf, "TOUCHOVERLAP:win1 (%d,%d) to (%d,%d)\n", win1->_begy, win1->_begx, win1->_begy + win1->_maxy, win1->_begx + win1->_maxx);
	fprintf(outf, "TOUCHOVERLAP:win2 (%d,%d) to (%d,%d)\n", win2->_begy, win2->_begx, win2->_begy + win2->_maxy, win2->_begx + win2->_maxx);
# endif
	if (starty >= endy || startx >= endx)
		return;
	starty -= win2->_begy;
	startx -= win2->_begx;
	endy -= win2->_begy;
	endx -= win2->_begx;
	endx--;
	for (y = starty; y < endy; y++)
		touchline(win2, y, startx, endx);
}
