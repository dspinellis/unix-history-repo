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
static char sccsid[] = "@(#)deleteln.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"curses.ext"

/*
 *	This routine deletes a line from the screen.  It leaves
 * (_cury,_curx) unchanged.
 *
 */
wdeleteln(win)
reg WINDOW	*win;
{
	reg char	*temp;
	reg int		y;
	reg char	*end;
	reg int		x;

# ifdef DEBUG
	fprintf(outf, "DELETELN(%0.2o)\n", win);
# endif
	temp = win->_y[win->_cury];
	for (y = win->_cury; y < win->_maxy - 1; y++) {
		if (win->_orig == NULL)
			win->_y[y] = win->_y[y + 1];
		else
			bcopy(win->_y[y + 1], win->_y[y], win->_maxx);
		touchline(win, y, 0, win->_maxx - 1);
	}
	if (win->_orig == NULL)
		win->_y[y] = temp;
	else
		temp = win->_y[y];
	for (end = &temp[win->_maxx]; temp < end; )
		*temp++ = ' ';
	touchline(win, win->_cury, 0, win->_maxx - 1);
	if (win->_orig == NULL)
		_id_subwins(win);
	return OK;
}
