/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)id_subwins.c	5.3 (Berkeley) 6/30/88";
#endif /* not lint */

# include	"curses.ext"

/*
 * _id_subwins:
 *	Re-sync the pointers to _y for all the subwindows.
 *
 */
_id_subwins(orig)
register WINDOW	*orig;
{
	register WINDOW	*win;
	register int	realy;
	register int	y, oy, x;

	realy = orig->_begy + orig->_cury;
	for (win = orig->_nextp; win != orig; win = win->_nextp) {
		/*
		 * If the window ends before our current position,
		 * don't need to do anything.
		 */
		if (win->_begy + win->_maxy <= realy)
			continue;

		oy = orig->_cury;
		for (y = realy - win->_begy; y < win->_maxy; y++, oy++)
			win->_y[y] = &orig->_y[oy][win->_ch_off];
	}
}
