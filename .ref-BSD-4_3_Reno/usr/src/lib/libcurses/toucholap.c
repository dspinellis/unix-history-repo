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
static char sccsid[] = "@(#)toucholap.c	5.4 (Berkeley) 6/1/90";
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
