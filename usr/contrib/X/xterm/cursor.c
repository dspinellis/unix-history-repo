#include <X/mit-copyright.h>

/* Copyright 1984, 1985   Massachusetts Institute of Technology		*/

/* cursor.c */


#ifndef lint
static char *rcsid_cursor_c = "$Header: cursor.c,v 10.9 86/02/01 16:06:14 tony Rel $";
#endif	lint

#include <X/Xlib.h>
#include <stdio.h>
#include "ptyx.h"

/*
 * Moves the cursor to the specified position, checking for bounds.
 * (this includes scrolling regions)
 * The origin is considered to be 0, 0 for this procedure.
 */
CursorSet(screen, row, col, flags)
register Screen	*screen;
register int	row, col;
unsigned	flags;
{
	register int maxr = screen->max_row;

	if (flags & ORIGIN) {
		row += screen->top_marg;
		maxr = screen->bot_marg;
	}

	row = (row < 0 ? 0 : row);
	col = (col < 0 ? 0 : col);
	screen->cur_col = (col <= screen->max_col ? col : screen->max_col);
	screen->cur_row = (row <= maxr ? row : maxr);
	screen->do_wrap = 0;
}

/*
 * moves the cursor left n, no wrap around
 */
CursorBack(screen, n)
register Screen	*screen;
int		n;
{
	screen->cur_col -= n;
	if (screen->cur_col < 0)
		screen->cur_col = 0;
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
	/* 
	 * indexing when below scrolling region is cursor down.
	 * if cursor high enough, no scrolling necessary.
	 */
	if (screen->cur_row > screen->bot_marg
	|| screen->cur_row + amount <= screen->bot_marg) {
		CursorDown(screen, amount);
		return;
	}

	Scroll(screen, amount - (screen->bot_marg - screen->cur_row));
	CursorDown(screen, screen->bot_marg - screen->cur_row);
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
 * Toggles cursor on or off at cursor position in screen.
 */
CursorToggle(screen, turnOn)
register Screen *screen;
int		turnOn;
{
	int fg = screen->foreground;
	int bg = screen->background;
	short flags = screen->buf [screen->cur_row] [screen->cur_col];
	char c;
	if (((turnOn ? INVERSEbit : 0) ^ (flags & INVERSEbit))) {
		fg = screen->background;
		bg = screen->cursorcolor;
	}
	/* If in normal mode repaint character */
	if (!screen->TekEmu) {
		if ((c = (flags & CHAR)) == 0) c = ' ';
		XText(screen->window, CursorX (screen), CursorY(screen), &c,1,
		    ((flags & BOLDbit) ? screen->fnt_bold : screen->fnt_norm),
		    fg, bg);
	/* If in Tek mode then invert */
	/* If in TekAmode then use TCursor instead of Cursor */
	} else if (screen->TekAMode)
		XPixFill(screen->window, TCursorX (screen), TCursorY(screen),
			screen->f_width, screen->f_height, screen->foreground,
			0, GXinvert, screen->xorplane);

	/* Avoid toggling cursor during Tektronix coordinate computations */
	else if (!screen->TekGMode)
		XPixFill(screen->window, CursorX (screen), CursorY(screen),
			screen->f_width, screen->f_height, screen->foreground,
			0, GXinvert, screen->xorplane);
}
