#include	<curses.h>

#define	min(a,b)	(a < b ? a : b)

main()
{
    WINDOW	*win1, *win2, *newwin();

    initscr();
    win1 = newwin(10, 40, 3, 1);	/* Origin at (y,x) = (3,1) and
					 * size of (dy,dx) = (10,40)	*/
    box(win1, '*', '*');

    win2 = newwin(LINES-1, COLS-1, 1, 1);  /* A large, blank window that
					    * gets the little one written
					    * onto it.			*/

    NewOverwrite(win1, win2);	/* These three lines write win1 onto win2, */
    wrefresh(win2);		/* refresh win2, and sleep... */
    sleep(2);

    mvwin(win1, 1, 3);		/* These four lines move win1, erase the */
    werase(win2);		/* old win2, write win1 in its new position */
    NewOverwrite(win1, win2);	/* onto win2, and redisplay win2.  Notice */
    wrefresh(win2);		/* irregularities in the border of win1. */
    sleep(2);

    mvwin(win1, 1, 5);		/* These four lines move win1 OUT OF the */
    werase(win2);		/* first 4 columns and it is displayed */
    NewOverwrite(win1, win2);	/* correctly, except for some traces of */
    wrefresh(win2);		/* the old win1's and a missing '*' near */
				/* the lower-right corner.		*/

    mvcur(0, COLS - 1, LINES - 1, 0);
    endwin();
}

NewOverwrite(win1, win2)
reg WINDOW	*win1, *win2;
{
    reg int		i, x, y, minx, miny, starty, startx;
/*
 *
 * minx and miny were erroneously computed before.
 * The following assignments do it correctly.
 *
 */
    miny = min(win1->_maxy, win2->_maxy + win2->_begy - win1->_begy);
    minx = min(win1->_maxx, win2->_maxx + win2->_begx - win1->_begx);

    starty = win1->_begy - win2->_begy;
    /*
     *
     * The following line was added:
     *
     */
    startx = win1->_begx - win2->_begx;

    for (y = 0; y < miny; y++)
    /*
     *
     * And this is the only other change...substituted startx
     * for 0 here ---------------------
     * 				      |
     *				      v				*/
	if (wmove(win2, y + starty, startx) != ERR)
	    for (x = 0; x < minx; x++)
		waddch(win2, win1->_y[y][x]);
}
