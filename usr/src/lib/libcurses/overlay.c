# include	"curses.ext"
# include	<ctype.h>

# define	min(a,b)	(a < b ? a : b)
# define	max(a,b)	(a < b ? a : b)

/*
 *	This routine writes win1 on win2 non-destructively.
 *
 * %G% (Berkeley) @(#)overlay.c	1.2
 */
overlay(win1, win2)
reg WINDOW	*win1, *win2; {

	reg char	*sp, *end;
	reg int		x, y, endy, endx, starty, startx, y_top,
			y_bot, x_left, x_right;

# ifdef DEBUG
	fprintf(outf, "OVERLAY(%0.2o, %0.2o);\n", win1, win2);
# endif
	y_top = max(win1->_begy, win2->_begy);
	y_bot = min(win1->_maxy, win2->_maxy);
	x_left = max(win1->_begx, win2->_begx);
	x_right = min(win1->_maxx, win2->_maxx);
	starty = y_top - win1->_begy;
	startx = x_left - win1->_begx;
	endy = y_bot - win1->_begy;
	endx = x_right - win1->_begx;
	for (y = starty; y < endy; y++) {
		end = &win1->_y[y][endx];
		x = startx + win1->_begx;
		for (sp = &win1->_y[y][startx]; sp <= end; sp++) {
			if (!isspace(*sp))
				mvwaddch(win2, y + win1->_begy, x, *sp);
			x++;
		}
	}
}
