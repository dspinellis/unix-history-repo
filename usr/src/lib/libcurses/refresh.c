/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)refresh.c	5.20 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>
#include <string.h>

static int curwin;
static short ly, lx;

WINDOW *_win;

static void	domvcur __P((int, int, int, int));
static int	makech __P((WINDOW *, int));
static void	quickch __P((WINDOW *));	
static void	scrolln __P((WINDOW *, int, int, int, int, int));

/*
 * wrefresh --
 *	Make the current screen look like "win" over the area coverd by
 *	win.
 */
int
wrefresh(win)
	register WINDOW *win;
{
	register __LINE *wlp;
	register int retval;
	register short wy;

	/* Make sure were in visual state. */
	if (__endwin) {
		tputs(VS, 0, __cputchar);
		tputs(TI, 0, __cputchar);
		__endwin = 0;
	}

	/* Initialize loop parameters. */

	ly = curscr->cury;
	lx = curscr->curx;
	wy = 0;
	_win = win;
	curwin = (win == curscr);

	if (!curwin)
		for (wy = 0; wy < win->maxy; wy++) {
			wlp = win->lines[wy];
			if (wlp->flags & __ISDIRTY)
				wlp->hash = 
				   __hash(wlp->line, win->maxx * __LDATASIZE);
		}

	if (win->flags & __CLEAROK || curscr->flags & __CLEAROK || curwin) {
		if ((win->flags & __FULLWIN) || curscr->flags & __CLEAROK) {
			tputs(CL, 0, __cputchar);
			ly = 0;
			lx = 0;
			if (!curwin) {
				curscr->flags &= ~__CLEAROK;
				curscr->cury = 0;
				curscr->curx = 0;
				werase(curscr);
			}
			__touchwin(win);
		}
		win->flags &= ~__CLEAROK;
	}
	if (!CA) {
		if (win->curx != 0)
			putchar('\n');
		if (!curwin)
			werase(curscr);
	}
#ifdef DEBUG
	__TRACE("wrefresh: (%0.2o): curwin = %d\n", win, curwin);
	__TRACE("wrefresh: \tfirstch\tlastch\n");
#endif

#ifndef NOQCH
	if (!__noqch && (win->flags & __FULLWIN) && !curwin)
    		quickch(win);
#endif
	for (wy = 0; wy < win->maxy; wy++) {
#ifdef DEBUG
		__TRACE("%d\t%d\t%d\n",
		    wy, win->lines[wy]->firstch, win->lines[wy]->lastch);
#endif
		if (!curwin)
			curscr->lines[wy]->hash = win->lines[wy]->hash;
		if (win->lines[wy]->flags & __ISDIRTY ||
		    win->lines[wy]->flags & __FORCEPAINT)
			if (makech(win, wy) == ERR)
				return (ERR);
			else {
				if (win->lines[wy]->firstch >= win->ch_off)
					win->lines[wy]->firstch = win->maxx +
					    win->ch_off;
				if (win->lines[wy]->lastch < win->maxx +
				    win->ch_off)
					win->lines[wy]->lastch = win->ch_off;
				if (win->lines[wy]->lastch < 
				    win->lines[wy]->firstch)
					win->lines[wy]->flags &= ~__ISDIRTY;
			}
#ifdef DEBUG
		__TRACE("\t%d\t%d\n", win->lines[wy]->firstch, 
			win->lines[wy]->lastch);
#endif
	}
	
#ifdef DEBUG
	__TRACE("refresh: ly=%d, lx=%d\n", ly, lx);
#endif

	if (win == curscr)
		domvcur(ly, lx, win->cury, win->curx);
	else {
		if (win->flags & __LEAVEOK) {
			curscr->cury = ly;
			curscr->curx = lx;
			ly -= win->begy;
			lx -= win->begx;
			if (ly >= 0 && ly < win->maxy && lx >= 0 &&
			    lx < win->maxx) {
				win->cury = ly;
				win->curx = lx;
			} else
				win->cury = win->curx = 0;
		} else {
			domvcur(ly, lx, win->cury + win->begy,
			    win->curx + win->begx);
			curscr->cury = win->cury + win->begy;
			curscr->curx = win->curx + win->begx;
		}
	}
	retval = OK;

	_win = NULL;
	(void)fflush(stdout);
	return (retval);
}

/*
 * makech --
 *	Make a change on the screen.
 */
static int
makech(win, wy)
	register WINDOW *win;
	int wy;
{
	register int nlsp, clsp;		/* Last space in lines. */
	register short wx, lch, y;
	register __LDATA *nsp, *csp, *cp;
	u_int force;
	char *ce;
	__LDATA blank = {' ', 0};

	/* Is the cursor still on the end of the last line? */
	if (wy > 0 && win->lines[wy - 1]->flags & __ISPASTEOL) {
		win->lines[wy - 1]->flags &= ~__ISPASTEOL;
		domvcur(ly, lx, ly + 1, 0);
		ly++;
		lx = 0;
	}
	if (!(win->lines[wy]->flags & __ISDIRTY))
		return (OK);
	wx = win->lines[wy]->firstch - win->ch_off;
	if (wx >= win->maxx)
		return (OK);
	else if (wx < 0)
		wx = 0;
	lch = win->lines[wy]->lastch - win->ch_off;
	if (lch < 0)
		return (OK);
	else if (lch >= win->maxx)
		lch = win->maxx - 1;
	y = wy + win->begy;

	if (curwin)
		csp = &blank;
	else
		csp = &curscr->lines[wy + win->begy]->line[wx + win->begx];

	nsp = &win->lines[wy]->line[wx];
	force = win->lines[wy]->flags & __FORCEPAINT;
	win->lines[wy]->flags &= ~__FORCEPAINT;
	if (CE && !curwin) {
		for (cp = &win->lines[wy]->line[win->maxx - 1]; 
		     cp->ch == ' ' && cp->attr == 0; cp--)
			if (cp <= win->lines[wy]->line)
				break;
		nlsp = cp - win->lines[wy]->line;
	}
	if (!curwin)
		ce = CE;
	else
		ce = NULL;

	if (force) {
		if (CM)
			tputs(tgoto(CM, lx, ly), 0, __cputchar);
		else {
			tputs(HO, 0, __cputchar);
			mvcur(0, 0, ly, lx);
		}
	}
	while (wx <= lch) {
		if (!force && bcmp(nsp, csp, sizeof(__LDATA)) == 0) {
			if (wx <= lch) {
				while (bcmp(nsp, csp, sizeof(__LDATA)) == 0 &&
			            wx <= lch) {
					    nsp++;
					    if (!curwin)
						    csp++;
					    ++wx;
				    }
				continue;
			}
			break;
		}
		domvcur(ly, lx, y, wx + win->begx);

#ifdef DEBUG
		__TRACE("makech: 1: wx = %d, ly= %d, lx = %d, newy = %d, newx = %d, force =%d\n", 
		    wx, ly, lx, y, wx + win->begx, force);
#endif
		ly = y;
		lx = wx + win->begx;
		while ((force || bcmp(nsp, csp, sizeof(__LDATA)) != 0) 
		    && wx <= lch) {
#ifdef notdef
			/* XXX
			 * The problem with this code is that we can't count on
			 * terminals wrapping around after the 
			 * last character on the previous line has been output
			 * In effect, what then could happen is that the CE 
			 * clear the previous line and do nothing to the
			 * next line.
			 */
			if (ce != NULL && wx >= nlsp && 
			    nsp->ch == ' ') {
				/* Check for clear to end-of-line. */
				ce = &curscr->lines[wy]->line[COLS - 1];
				while (ce->ch == ' ' && ce->attr = 0)
					if (ce-- <= csp)
						break;
				clsp = ce - curscr->lines[wy]->line - 
				       win->begx * __LDATASIZE;
#ifdef DEBUG
			__TRACE("makech: clsp = %d, nlsp = %d\n", clsp, nlsp);
#endif
				if (clsp - nlsp >= strlen(CE) 
				    && clsp < win->maxx * __LDATASIZE) {
#ifdef DEBUG
					__TRACE("makech: using CE\n");
#endif
					tputs(CE, 0, __cputchar);
					lx = wx + win->begx;
					while (wx++ <= clsp) {
						csp->ch = ' ';
						csp->attr = 0;
						csp++;
					}
					return (OK);
				}
				ce = NULL;
			}
#endif

			/* Enter/exit standout mode as appropriate. */
			if (SO && (nsp->attr & __STANDOUT) !=
			    (curscr->flags & __WSTANDOUT)) {
				if (nsp->attr & __STANDOUT) {
					tputs(SO, 0, __cputchar);
					curscr->flags |= __WSTANDOUT;
				} else {
					tputs(SE, 0, __cputchar);
					curscr->flags &= ~__WSTANDOUT;
				}
			}

			wx++;
			if (wx >= win->maxx && wy == win->maxy - 1 && !curwin)
				if (win->flags & __SCROLLOK) {
					if (curscr->flags & __WSTANDOUT
					    && win->flags & __ENDLINE)
						if (!MS) {
							tputs(SE, 0,
							    __cputchar);
							curscr->flags &=
							    ~__WSTANDOUT;
						}
					if (!curwin) {
						csp->attr = nsp->attr;
						putchar(csp->ch = nsp->ch);
					} else
						putchar(nsp->ch);
#ifdef notdef /* XXX why is this here? */
					if (win->flags & __FULLWIN && !curwin)
						scroll(curscr);
#endif
					if (wx + win->begx < curscr->maxx) {
						domvcur(ly, wx + win->begx, 
						    win->begy + win->maxy - 1,
						    win->begx + win->maxx - 1);
					}
					ly = win->begy + win->maxy - 1;
					lx = win->begx + win->maxx - 1;
					return (OK);
				} else
					if (win->flags & __SCROLLWIN) {
						lx = --wx;
						return (ERR);
					}
			if (!curwin) {
				csp->attr = nsp->attr;
				putchar(csp->ch = nsp->ch);
				csp++;
		       	} else
				putchar(nsp->ch);
			
#ifdef DEBUG
			__TRACE("makech: putchar(%c)\n", nsp->ch & 0177);
#endif
			if (UC && (nsp->attr & __STANDOUT)) {
				putchar('\b');
				tputs(UC, 0, __cputchar);
			}
			nsp++;
		}
#ifdef DEBUG
		__TRACE("makech: 2: wx = %d, lx = %d\n", wx, lx);
#endif
		if (lx == wx + win->begx)	/* If no change. */
			break;
		lx = wx + win->begx;
		if (lx >= COLS && AM) {
			/*
			 * xn glitch: chomps a newline after auto-wrap.
			 * we just feed it now and forget about it.
			 */
			if (XN) {
				lx = 0;
				ly++;
				putchar('\n');
				putchar('\r');
			} else {
				if (wy != LINES)
					win->lines[wy]->flags |= __ISPASTEOL;
				lx = COLS - 1;
			}
		} else if (wx >= win->maxx) {
			if (wy != win->maxy)
				win->lines[wy]->flags |= __ISPASTEOL;
			domvcur(ly, lx, ly, win->maxx + win->begx - 1);
			lx = win->maxx + win->begx - 1;
		}

#ifdef DEBUG
		__TRACE("makech: 3: wx = %d, lx = %d\n", wx, lx);
#endif
	}
	return (OK);
}

/*
 * domvcur --
 *	Do a mvcur, leaving standout mode if necessary.
 */
static void
domvcur(oy, ox, ny, nx)
	int oy, ox, ny, nx;
{
	if (curscr->flags & __WSTANDOUT && !MS) {
		tputs(SE, 0, __cputchar);
		curscr->flags &= ~__WSTANDOUT;
	}

	mvcur(oy, ox, ny, nx);
}

/*
 * Quickch() attempts to detect a pattern in the change of the window
 * in order to optimize the change, e.g., scroll n lines as opposed to 
 * repainting the screen line by line.
 */

static void
quickch(win)
	WINDOW *win;
{
#define THRESH		win->maxy / 4

	register __LINE *clp, *tmp1, *tmp2;
	register int bsize, curs, curw, starts, startw, i, j;
	int n, target, cur_period, bot, top, sc_region;
	__LDATA buf[1024];
	u_int blank_hash;

	/*
	 * Search for the largest block of text not changed.
	 * Invariants of the loop:
	 * - Startw is the index of the beginning of the examined block in win.
         * - Starts is the index of the beginning of the examined block in 
	 *    curscr.
	 * - Curs is the index of one past the end of the exmined block in win.
	 * - Curw is the index of one past the end of the exmined block in 
	 *   curscr.
	 * - bsize is the current size of the examined block.
         */
	for (bsize = win->maxy; bsize >= THRESH; bsize--)
		for (startw = 0; startw <= win->maxy - bsize; startw++)
			for (starts = 0; starts <= win->maxy - bsize; 
			     starts++) {
				for (curw = startw, curs = starts;
				     curs < starts + bsize; curw++, curs++)
					if (win->lines[curw]->flags &
					    __FORCEPAINT ||
					    (win->lines[curw]->hash !=
					    curscr->lines[curs]->hash ||
				            bcmp(win->lines[curw]->line, 
					    curscr->lines[curs]->line, 
					    win->maxx * __LDATASIZE) != 0))
						break;
				if (curs == starts + bsize)
					goto done;
			}
 done:
	/* Did not find anything */
	if (bsize < THRESH)	
		return;

	/* 
	 * Find how many lines from the top of the screen are unchanged.
	 */
	if (starts != 0) {
		for (top = 0; top < win->maxy; top++)
			if (win->lines[top]->flags & __FORCEPAINT ||
			    win->lines[top]->hash != curscr->lines[top]->hash 
			    || bcmp(win->lines[top]->line, 
			    curscr->lines[top]->line, 
			    win->maxx * __LDATASIZE) != 0)
				break;
	} else
		top = 0;
	
       /*
	* Find how many lines from bottom of screen are unchanged. 
	*/
	if (curs != win->maxy) {
		for (bot = win->maxy - 1; bot >= 0; bot--)
			if (win->lines[bot]->flags & __FORCEPAINT ||
			    win->lines[bot]->hash != curscr->lines[bot]->hash 
			    || bcmp(win->lines[bot]->line, 
			    curscr->lines[bot]->line, 
			    win->maxx * __LDATASIZE) != 0)
				break;
	} else
		bot = win->maxy - 1;

#ifdef DEBUG
	__TRACE("quickch:bsize=%d,starts=%d,startw=%d,curw=%d,curs=%d,top=%d,bot=%d\n", 
		bsize, starts, startw, curw, curs, top, bot);
#endif

	/* 
	 * Make sure that there is no overlap between the bottom and top 
	 * regions and the middle scrolled block.
	 */
	if (bot < curw)
		bot = curw - 1;
	if (top > startw)
		top = startw;

	n = startw - starts;

	if (n != 0)
		scrolln(win, starts, startw, curs, curw, top);


	/* So we don't have to call __hash() each time */
	for (i = 0; i < win->maxx; i++) {
		buf[i].ch = ' ';
		buf[i].attr = 0;
	}
	blank_hash = __hash(buf, win->maxx * __LDATASIZE);

	/*
	 * Perform the rotation to maintain the consistency of curscr.
	 * This is hairy!
	 * Invariants of the loop:
	 * - I is the index of the current line.
	 * - Target is the index of the target of line i.
	 * - Tmp1 points to current line (i).
	 * - Tmp2 and points to target line (target);
	 * - Cur_period is the index of the end of the current period. 
	 *   (see below).
	 *
	 * There are 2 major issues here that make this rotation non-trivial:
	 * 1.  Scrolling in a scrolling region bounded by the top
	 *     and bottom regions determined (whose size is sc_region).
	 * 2.  As a result of the use of the mod function, there may be a 
	 *     period introduced, i.e., 2 maps to 4, 4 to 6, n-2 to 0, and
	 *     0 to 2, which then causes all odd lines not to be rotated.
	 *     To remedy this, an index of the end ( = beginning) of the 
	 *     current 'period' is kept, cur_period, and when it is reached, 
	 *     the next period is started from cur_period + 1 which is 
	 *     guaranteed not to have been reached since that would mean that
	 *     all records would have been reached. (think about it...).
	 * 
	 * Lines in the rotation can have 3 attributes which are marked on the
	 * line so that curscr is consistent with the visual screen.
	 * 1.  Not dirty -- lines inside the scrolling region, top region or
	 *                  bottom region.
	 * 2.  Blank lines -- lines in the differential of scrolled block 
	 *                    between win and curscr in the scrolling region.
	 *
	 * 3.  Dirty line -- all other lines are marked dirty.
	 */
	sc_region = bot - top + 1;
	i = top;
	tmp1 = curscr->lines[top];
	cur_period = top;
	for (j = top; j <= bot; j++) {
		target = (i - top + n + sc_region) % sc_region + top;
		tmp2 = curscr->lines[target];
		curscr->lines[target] = tmp1;
		/* Mark block as clean and blank out scrolled lines. */
		clp = curscr->lines[target];
#ifdef DEBUG
		__TRACE("quickch: n=%d startw=%d curw=%d i = %d target=%d ",
			n, startw, curw, i, target);
#endif
		if (target >= startw && target < curw || target < top || 
		    target > bot) {
#ifdef DEBUG
			__TRACE("-- notdirty");
#endif
			win->lines[target]->flags &= ~__ISDIRTY;
		} else if ((n < 0 && target >= curw && target < curs) ||
		           (n > 0 && target < startw && target >= starts)) {
			if (clp->hash != blank_hash ||  bcmp(clp->line, 
			    buf, win->maxx * __LDATASIZE) !=0) {
				(void)bcopy(buf, clp->line,  
				    win->maxx * __LDATASIZE);
#ifdef DEBUG
				__TRACE("-- blanked out: dirty");
#endif
				clp->hash = blank_hash;
				__touchline(win, target, 0, win->maxx - 1, 0);
			} else 
				__touchline(win, target, 0, win->maxx - 1, 0);
#ifdef DEBUG
				__TRACE(" -- blank line already: dirty");
#endif
		} else {
#ifdef DEBUG
			__TRACE(" -- dirty");
#endif
			__touchline(win, target, 0, win->maxx - 1, 0);
		}
#ifdef DEBUG
		__TRACE("\n");
#endif
		if (target == cur_period) {
			i = target + 1;
			tmp1 = curscr->lines[i];
			cur_period = i;
		} else {
			tmp1 = tmp2;
			i = target;
		}
	}
#ifdef DEBUG
		__TRACE("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n");
		for (i = 0; i < curscr->maxy; i++) {
			__TRACE("Q: %d:", i);
			for (j = 0; j < curscr->maxx; j++) 
				__TRACE("%c", 
			           curscr->lines[i]->line[j].ch);
			__TRACE("\n");
		}
#endif
}

/*
 * Scrolln performs the scroll by n lines, where n is starts - startw.
 */
static void
scrolln(win, starts, startw, curs, curw, top)
	WINDOW *win;
	int starts, startw, curs, curw, top;
{
	int i, oy, ox, n;

	oy = curscr->cury;
	ox = curscr->curx;
	n = starts - startw;

	if (n > 0) {
		mvcur(oy, ox, top, 0);
		/* Scroll up the block */
		if (DL)
			tputs(tscroll(DL, n), 0, __cputchar);
		else
			for(i = 0; i < n; i++)
				tputs(dl, 0, __cputchar);

		/* 
		 * Push down the bottom region.
		 */
		mvcur(top, 0, curw, 0);
		if (AL)
			tputs(tscroll(AL, n), 0, __cputchar);
		else
			for(i = 0; i < n; i++)
				tputs(al, 0, __cputchar);
		mvcur(curw, 0, oy, ox);
	} else {
		/* Preserve the bottom lines */
		mvcur(oy, ox, curs, 0);
		if (DL)
			tputs(tscroll(DL, -n), 0, __cputchar);
		else
		       	for(i = n; i < 0; i++)
				tputs(dl, 0, __cputchar);
		mvcur(curs, 0, starts, 0);

		/* Scroll the block down */
		if (AL)
			tputs(tscroll(AL, -n), 0, __cputchar);
		else
			for(i = n; i < 0; i++)
				tputs(al, 0, __cputchar);
		mvcur(starts, 0, oy, ox);
	}		
}


