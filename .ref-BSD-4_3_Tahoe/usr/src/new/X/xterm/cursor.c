/*
 *	$Source: /u1/X/xterm/RCS/cursor.c,v $
 *	$Header: cursor.c,v 10.100 86/12/01 14:43:54 jg Rel $
 */

#ifndef lint
static char *rcsid_cursor_c = "$Header: cursor.c,v 10.100 86/12/01 14:43:54 jg Rel $";
#endif	lint

#include <X/mit-copyright.h>

/* Copyright 1984, 1985   Massachusetts Institute of Technology		*/

/* cursor.c */


#ifndef lint
static char sccs_id[] = "@(#)cursor.c\tX10/6.6B\t12/26/86";
#endif	lint

#include <X/Xlib.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include "scrollbar.h"
#include "ptyx.h"

extern Terminal term;

/*
 * Moves the cursor to the specified position, checking for bounds.
 * (this includes scrolling regions)
 * The origin is considered to be 0, 0 for this procedure.
 * In the status line, the cursor moves only horizontally.
 */
CursorSet(screen, row, col, flags)
register Screen	*screen;
register int	row, col;
unsigned	flags;
{
	register int maxr;

	col = (col < 0 ? 0 : col);
	screen->cur_col = (col <= screen->max_col ? col : screen->max_col);
	if(!screen->instatus) {
		maxr = screen->max_row;
		if (flags & ORIGIN) {
			row += screen->top_marg;
			maxr = screen->bot_marg;
		}
		row = (row < 0 ? 0 : row);
		screen->cur_row = (row <= maxr ? row : maxr);
	}
	screen->do_wrap = 0;
}

/*
 * moves the cursor left n, no wrap around
 */
CursorBack(screen, n)
register Screen	*screen;
int		n;
{
	register int i, j, k, rev;

	if((rev = (term.flags & (REVERSEWRAP | WRAPAROUND)) ==
	 (REVERSEWRAP | WRAPAROUND)) && screen->do_wrap)
		n--;
	if ((screen->cur_col -= n) < 0) {
		if(rev) {
			if((i = (j = screen->max_col + 1) * screen->cur_row +
			 screen->cur_col) < 0) {
				k = j * (screen->max_row + 1);
				i += ((-i) / k + 1) * k;
			}
			screen->cur_row = i / j;
			screen->cur_col = i % j;
		} else
			screen->cur_col = 0;
	}
	screen->do_wrap = 0;
}

/*
 * moves the cursor forward n, no wraparound
 */
CursorForward(screen, n)
register Screen	*screen;
int		n;
{
	screen->cur_col += n;
	if (screen->cur_col > screen->max_col)
		screen->cur_col = screen->max_col;
	screen->do_wrap = 0;
}

/* 
 * moves the cursor down n, no scrolling.
 * Won't pass bottom margin or bottom of screen.
 */
CursorDown(screen, n)
register Screen	*screen;
int		n;
{
	register int max;

	max = (screen->cur_row > screen->bot_marg ?
		screen->max_row : screen->bot_marg);

	screen->cur_row += n;
	if (screen->cur_row > max)
		screen->cur_row = max;
	screen->do_wrap = 0;
}

/* 
 * moves the cursor up n, no linestarving.
 * Won't pass top margin or top of screen.
 */
CursorUp(screen, n)
register Screen	*screen;
int		n;
{
	register int min;

	min = (screen->cur_row < screen->top_marg ?
		0 : screen->top_marg);

	screen->cur_row -= n;
	if (screen->cur_row < min)
		screen->cur_row = min;
	screen->do_wrap = 0;
}

/* 
 * Moves cursor down amount lines, scrolls if necessary.
 * Won't leave scrolling region. No carriage return.
 */
Index(screen, amount)
register Screen	*screen;
register int	amount;
{
	register int lines, j;
	register char *str;
	int n;
	XEvent ev;

	/* 
	 * indexing when below scrolling region is cursor down.
	 * if cursor high enough, no scrolling necessary.
	 */
	if (screen->cur_row > screen->bot_marg
	 || screen->cur_row + amount <= screen->bot_marg) {
		if(screen->pagemode)
			screen->pagecnt += amount;
		CursorDown(screen, amount);
		return;
	}

	CursorDown(screen, j = screen->bot_marg - screen->cur_row);
	amount -= j;
	if((lines = screen->bot_marg - screen->top_marg - screen->pageoverlap)
	 <= 0)
		lines = 1;
	if(!screen->pagemode || (amount + screen->pagecnt) <= lines) {
		if(screen->pagemode)
			screen->pagecnt += amount;
		Scroll(screen, amount);
		return;
	}
	ioctl(screen->respond, TIOCSTOP, NULL);
	if(screen->cursor_state)
		HideCursor();
	if((j = lines - screen->pagecnt) > 0) {
		Scroll(screen, j);
		amount -= j;
	}
	do {
		if(screen->scroll_amt)
			FlushScroll(screen);
		j = FALSE;
		do {
			XNextEvent(&ev);
			switch((int)ev.type) {
			 case KeyPressed:
				str = XLookupMapping(&ev, &n);
				if(n > 0) {
					if(*str == '\r')
						screen->pagecnt = (lines - 1);
					else if(*str < ' ' || *str == '\177') {
						screen->pagecnt = 0;
						Input(&term.keyboard, screen,
						 &ev);
						ioctl(screen->respond, TIOCSTOP,
						 NULL);
					} else
						screen->pagecnt = 0;
				} else
					screen->pagecnt = 0;
				j = TRUE;
				break;
			 case ButtonPressed:
			 case ButtonReleased:
				screen->pagecnt = amount;
				xeventpass(&ev);
				if(!screen->pagemode) {
					Scroll(screen, amount);
					ioctl(screen->respond, TIOCSTART, NULL);
					return;
				}
				break;
			 default:
				xeventpass(&ev);
				break;
			}
		} while(!j);
		j = lines - screen->pagecnt;
		if(j > amount)
			j = amount;
		Scroll(screen, j);
		screen->pagecnt += j;
	} while((amount -= j) > 0);
	ioctl(screen->respond, TIOCSTART, NULL);
}

/*
 * Moves cursor up amount lines, reverse scrolls if necessary.
 * Won't leave scrolling region. No carriage return.
 */
RevIndex(screen, amount)
register Screen	*screen;
register int	amount;
{
	/*
	 * reverse indexing when above scrolling region is cursor up.
	 * if cursor low enough, no reverse indexing needed
	 */
	if (screen->cur_row < screen->top_marg
	 || screen->cur_row-amount >= screen->top_marg) {
		CursorUp(screen, amount);
		return;
	}

	RevScroll(screen, amount - (screen->cur_row - screen->top_marg));
	CursorUp(screen, screen->cur_row - screen->top_marg);
}

/*
 * Moves Cursor To First Column In Line
 */
CarriageReturn(screen)
register Screen *screen;
{
	screen->cur_col = 0;
	screen->do_wrap = 0;
}

/*
 * Save Cursor and Attributes
 */
CursorSave(term, sc)
register Terminal *term;
register SavedCursor *sc;
{
	register Screen *screen = &term->screen;

	sc->row = screen->cur_row;
	sc->col = screen->cur_col;
	sc->flags = term->flags;
	sc->curgl = screen->curgl;
	sc->curgr = screen->curgr;
	bcopy(screen->gsets, sc->gsets, sizeof(screen->gsets));
}

/*
 * Restore Cursor and Attributes
 */
CursorRestore(term, sc)
register Terminal *term;
register SavedCursor *sc;
{
	register Screen *screen = &term->screen;

	bcopy(sc->gsets, screen->gsets, sizeof(screen->gsets));
	screen->curgl = sc->curgl;
	screen->curgr = sc->curgr;
	term->flags &= ~(BOLD|INVERSE|UNDERLINE);
	term->flags |= sc->flags & (BOLD|INVERSE|UNDERLINE);
	CursorSet(screen, sc->row, sc->col, term->flags);
}
