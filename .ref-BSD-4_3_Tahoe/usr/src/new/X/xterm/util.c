/*
 *	$Source: /u1/X/xterm/RCS/util.c,v $
 *	$Header: util.c,v 10.100 86/12/01 14:45:43 jg Rel $
 */

#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

/* util.c */

#ifndef lint
static char sccs_id[] = "@(#)util.c\tX10/6.6B\t12/26/86";
#endif	lint

#include <stdio.h>
#include <X/Xlib.h>
#include <signal.h>
#include <setjmp.h>
typedef int *jmp_ptr;

#include "scrollbar.h"
#include "ptyx.h"
#include "data.h"
#include "error.h"

/*
 * These routines are used for the jump scroll feature
 */
FlushScroll(screen)
register Screen *screen;
{
	register int i;
	register int shift = -screen->topline;
	register int bot = screen->max_row - shift;
	register int refreshtop;
	register int refreshheight;
	register int scrolltop;
	register int scrollheight;

	if(screen->cursor_state)
		HideCursor();
	if(screen->scroll_amt > 0) {
		refreshheight = screen->refresh_amt;
		scrollheight = screen->bot_marg - screen->top_marg -
		 refreshheight + 1;
		if((refreshtop = screen->bot_marg - refreshheight + 1 + shift) >
		 (i = screen->max_row - screen->scroll_amt + 1))
			refreshtop = i;
		if(screen->sb && GetSaveState(screen->sb) &&
		 screen->top_marg == 0) {
			scrolltop = 0;
			if((scrollheight += shift) > i)
				scrollheight = i;
			if((i = screen->bot_marg - bot) > 0 &&
			 (refreshheight -= i) < screen->scroll_amt)
				refreshheight = screen->scroll_amt;
			if((i = -GetScrollBarTop(screen->sb)) <
			 screen->savelines) {
				if((i += screen->scroll_amt) >
				 screen->savelines)
					i = screen->savelines;
				SetScrollBarTop(screen->sb, -i);
				DrawScrollRegion(screen->sb);
			}
		} else {
			scrolltop = screen->top_marg + shift;
			if((i = bot - (screen->bot_marg - screen->refresh_amt +
			 screen->scroll_amt)) > 0) {
				if(bot < screen->bot_marg)
					refreshheight = screen->scroll_amt + i;
			} else {
				scrollheight += i;
				refreshheight = screen->scroll_amt;
				if((i = screen->top_marg + screen->scroll_amt -
				 1 - bot) > 0) {
					refreshtop += i;
					refreshheight -= i;
				}
			}
		}
	} else {
		refreshheight = -screen->refresh_amt;
		scrollheight = screen->bot_marg - screen->top_marg -
		 refreshheight + 1;
		refreshtop = screen->top_marg + shift;
		scrolltop = refreshtop + refreshheight;
		if((i = screen->bot_marg - bot) > 0)
			scrollheight -= i;
		if((i = screen->top_marg + refreshheight - 1 - bot) > 0)
			refreshheight -= i;
	}
	if(scrollheight > 0) {
		if (screen->multiscroll && scrollheight == 1 &&
		 screen->topline == 0 && screen->top_marg == 0 &&
		 screen->bot_marg == screen->max_row) {
			if (screen->incopy < 0 && screen->scrolls == 0)
				CopyWait (screen);
			screen->scrolls++;
		} else {
			if (screen->incopy)
				CopyWait (screen);
			screen->incopy = -1;
		}

		XMoveArea (VWindow(screen), screen->border, (scrolltop +
		 screen->scroll_amt) * FontHeight(screen) + screen->border +
		 Titlebar(screen), screen->border, scrolltop * FontHeight(screen)
		 + screen->border + Titlebar(screen), Width(screen),
		 scrollheight * FontHeight(screen));
	}
	screen->scroll_amt = 0;
	screen->refresh_amt = 0;
	if(refreshheight > 0) {
		XTileSet (VWindow(screen), screen->border, refreshtop *
		 FontHeight(screen) + screen->border + Titlebar(screen),
		 Width(screen), refreshheight * FontHeight(screen),
		 screen->bgndtile);
		ScrnRefresh(screen, refreshtop, 0, refreshheight,
		 screen->max_col + 1);
	}
}

AddToRefresh(screen)
register Screen *screen;
{
	register int amount = screen->refresh_amt;
	register int row = screen->cur_row;

	if(amount == 0 || screen->instatus)
		return(0);
	if(amount > 0) {
		register int bottom;

		if(row == (bottom = screen->bot_marg) - amount) {
			screen->refresh_amt++;
			return(1);
		}
		return(row >= bottom - amount + 1 && row <= bottom);
	} else {
		register int top;

		amount = -amount;
		if(row == (top = screen->top_marg) + amount) {
			screen->refresh_amt--;
			return(1);
		}
		return(row <= top + amount - 1 && row >= top);
	}
}

/* 
 * scrolls the screen by amount lines, erases bottom, doesn't alter 
 * cursor position (i.e. cursor moves down amount relative to text).
 * All done within the scrolling region, of course. 
 * requires: amount > 0
 */
Scroll (screen, amount)
register Screen *screen;
register int amount;
{
	register int i = screen->bot_marg - screen->top_marg + 1;
	register int shift;
	register int bot;
	register int refreshtop;
	register int refreshheight;
	register int scrolltop;
	register int scrollheight;

	if(screen->cursor_state)
		HideCursor();
	if (amount > i)
		amount = i;
    if(screen->jumpscroll) {
	if(screen->scroll_amt > 0) {
		if(screen->refresh_amt + amount > i)
			FlushScroll(screen);
		screen->scroll_amt += amount;
		screen->refresh_amt += amount;
	} else {
		if(screen->scroll_amt < 0)
			FlushScroll(screen);
		screen->scroll_amt = amount;
		screen->refresh_amt = amount;
	}
	refreshheight = 0;
    } else {

	if (amount == i) {
		ClearScreen(screen);
		return;
	}
	shift = -screen->topline;
	bot = screen->max_row - shift;
	scrollheight = i - amount;
	refreshheight = amount;
	if((refreshtop = screen->bot_marg - refreshheight + 1 + shift) >
	 (i = screen->max_row - refreshheight + 1))
		refreshtop = i;
	if(screen->sb && GetSaveState(screen->sb) && screen->top_marg == 0) {
		scrolltop = 0;
		if((scrollheight += shift) > i)
			scrollheight = i;
		if((i = -GetScrollBarTop(screen->sb)) < screen->savelines) {
			if((i += amount) > screen->savelines)
				i = screen->savelines;
			SetScrollBarTop(screen->sb, -i);
			DrawScrollRegion(screen->sb);
		}
	} else {
		scrolltop = screen->top_marg + shift;
		if((i = screen->bot_marg - bot) > 0) {
			scrollheight -= i;
			if((i = screen->top_marg + amount - 1 - bot) >= 0) {
				refreshtop += i;
				refreshheight -= i;
			}
		}
	}
	if(scrollheight > 0) {
		if (screen->multiscroll
		&& amount==1 && screen->topline == 0
		&& screen->top_marg==0
		&& screen->bot_marg==screen->max_row) {
			if (screen->incopy<0 && screen->scrolls==0)
				CopyWait(screen);
			screen->scrolls++;
		} else {
			if (screen->incopy)
				CopyWait(screen);
			screen->incopy = -1;
		}

		XMoveArea(VWindow(screen), screen->border, (scrolltop+amount) *
		 FontHeight(screen) + screen->border + Titlebar(screen),
		 screen->border, scrolltop * FontHeight(screen) + screen->border
		 + Titlebar(screen), Width(screen), scrollheight *
		 FontHeight(screen));
	}
	if(refreshheight > 0) {
		XTileSet (VWindow(screen), screen->border, refreshtop *
		 FontHeight(screen) + screen->border + Titlebar(screen),
		 Width(screen), refreshheight * FontHeight(screen),
		 screen->bgndtile);
		if(refreshheight > shift)
			refreshheight = shift;
	}
    }
	if(screen->sb && GetSaveState(screen->sb) && screen->top_marg == 0)
		ScrnDeleteLine(screen->allbuf, screen->bot_marg +
		 screen->savelines, 0, amount, screen->max_col + 1);
	else
		ScrnDeleteLine(screen->buf, screen->bot_marg, screen->top_marg,
		 amount, screen->max_col + 1);
	if(refreshheight > 0)
		ScrnRefresh(screen, refreshtop, 0, refreshheight,
		 screen->max_col + 1);
}


/*
 * Reverse scrolls the screen by amount lines, erases top, doesn't alter
 * cursor position (i.e. cursor moves up amount relative to text).
 * All done within the scrolling region, of course.
 * Requires: amount > 0
 */
RevScroll(screen, amount)
register Screen *screen;
register int amount;
{
	register int i = screen->bot_marg - screen->top_marg + 1;
	register int shift;
	register int bot;
	register int refreshtop;
	register int refreshheight;
	register int scrolltop;
	register int scrollheight;

	if(screen->cursor_state)
		HideCursor();
	if (amount > i)
		amount = i;
    if(screen->jumpscroll) {
	if(screen->scroll_amt < 0) {
		if(-screen->refresh_amt + amount > i)
			FlushScroll(screen);
		screen->scroll_amt -= amount;
		screen->refresh_amt -= amount;
	} else {
		if(screen->scroll_amt > 0)
			FlushScroll(screen);
		screen->scroll_amt = -amount;
		screen->refresh_amt = -amount;
	}
    } else {
	shift = -screen->topline;
	bot = screen->max_row - shift;
	refreshheight = amount;
	scrollheight = screen->bot_marg - screen->top_marg -
	 refreshheight + 1;
	refreshtop = screen->top_marg + shift;
	scrolltop = refreshtop + refreshheight;
	if((i = screen->bot_marg - bot) > 0)
		scrollheight -= i;
	if((i = screen->top_marg + refreshheight - 1 - bot) > 0)
		refreshheight -= i;
	if(scrollheight > 0) {
		if (screen->multiscroll
		&& amount==1 && screen->topline == 0
		&& screen->top_marg==0
		&& screen->bot_marg==screen->max_row) {
			if (screen->incopy<0 && screen->scrolls==0)
				CopyWait(screen);
			screen->scrolls++;
		} else {
			if (screen->incopy)
				CopyWait(screen);
			screen->incopy = -1;
		}

		XMoveArea (VWindow(screen), screen->border, (scrolltop-amount) *
		 FontHeight(screen) + screen->border + Titlebar(screen),
		 screen->border, scrolltop * FontHeight(screen) + screen->border
		 + Titlebar(screen), Width(screen), scrollheight *
		 FontHeight(screen));
	}
	if(refreshheight > 0)
		XTileSet (VWindow(screen), screen->border, refreshtop *
		 FontHeight(screen) + screen->border + Titlebar(screen),
		 Width(screen), refreshheight * FontHeight(screen),
		 screen->bgndtile);
    }
	ScrnInsertLine (screen->buf, screen->bot_marg, screen->top_marg,
			amount, screen->max_col + 1);
}

/*
 * If cursor not in scrolling region, returns.  Else,
 * inserts n blank lines at the cursor's position.  Lines above the
 * bottom margin are lost.
 */
InsertLine (screen, n)
register Screen *screen;
register int n;
{
	register int i;
	register int shift;
	register int bot;
	register int refreshtop;
	register int refreshheight;
	register int scrolltop;
	register int scrollheight;

	if (screen->cur_row < screen->top_marg ||
	 screen->cur_row > screen->bot_marg)
		return;
	if(screen->cursor_state)
		HideCursor();
	screen->do_wrap = 0;
	if (n > (i = screen->bot_marg - screen->cur_row + 1))
		n = i;
    if(screen->jumpscroll) {
	if(screen->scroll_amt <= 0 &&
	 screen->cur_row <= -screen->refresh_amt) {
		if(-screen->refresh_amt + n > screen->max_row + 1)
			FlushScroll(screen);
		screen->scroll_amt -= n;
		screen->refresh_amt -= n;
	} else if(screen->scroll_amt)
		FlushScroll(screen);
    }
    if(!screen->scroll_amt) {
	shift = -screen->topline;
	bot = screen->max_row - shift;
	refreshheight = n;
	scrollheight = screen->bot_marg - screen->cur_row - refreshheight + 1;
	refreshtop = screen->cur_row + shift;
	scrolltop = refreshtop + refreshheight;
	if((i = screen->bot_marg - bot) > 0)
		scrollheight -= i;
	if((i = screen->cur_row + refreshheight - 1 - bot) > 0)
		refreshheight -= i;
	if(scrollheight > 0) {
		if (screen->incopy)
			CopyWait (screen);
		screen->incopy = -1;
		XMoveArea (VWindow(screen), screen->border, (scrolltop - n) *
		 FontHeight(screen) + screen->border + Titlebar(screen),
		 screen->border, scrolltop * FontHeight(screen) + screen->border
		 + Titlebar(screen), Width(screen), scrollheight *
		 FontHeight(screen));
	}
	if(refreshheight > 0)
		XTileSet (VWindow(screen),
		 screen->border, refreshtop * FontHeight(screen) + screen->border
		 + Titlebar(screen), Width(screen), refreshheight *
		 FontHeight(screen), screen->bgndtile);
    }
	/* adjust screen->buf */
	ScrnInsertLine(screen->buf, screen->bot_marg, screen->cur_row, n,
			screen->max_col + 1);
}

/*
 * If cursor not in scrolling region, returns.  Else, deletes n lines
 * at the cursor's position, lines added at bottom margin are blank.
 */
DeleteLine(screen, n)
register Screen *screen;
register int n;
{
	register int i;
	register int shift;
	register int bot;
	register int refreshtop;
	register int refreshheight;
	register int scrolltop;
	register int scrollheight;

	if (screen->cur_row < screen->top_marg ||
	 screen->cur_row > screen->bot_marg)
		return;
	if(screen->cursor_state)
		HideCursor();
	screen->do_wrap = 0;
	if (n > (i = screen->bot_marg - screen->cur_row + 1))
		n = i;
    if(screen->jumpscroll) {
	if(screen->scroll_amt >= 0 && screen->cur_row == screen->top_marg) {
		if(screen->refresh_amt + n > screen->max_row + 1)
			FlushScroll(screen);
		screen->scroll_amt += n;
		screen->refresh_amt += n;
	} else if(screen->scroll_amt)
		FlushScroll(screen);
    }
    if(!screen->scroll_amt) {

	shift = -screen->topline;
	bot = screen->max_row - shift;
	scrollheight = i - n;
	refreshheight = n;
	if((refreshtop = screen->bot_marg - refreshheight + 1 + shift) >
	 (i = screen->max_row - refreshheight + 1))
		refreshtop = i;
	if(screen->sb && GetSaveState(screen->sb) && screen->cur_row == 0) {
		scrolltop = 0;
		if((scrollheight += shift) > i)
			scrollheight = i;
		if((i = -GetScrollBarTop(screen->sb)) < screen->savelines) {
			if((i += n) > screen->savelines)
				i = screen->savelines;
			SetScrollBarTop(screen->sb, -i);
			DrawScrollRegion(screen->sb);
		}
	} else {
		scrolltop = screen->cur_row + shift;
		if((i = screen->bot_marg - bot) > 0) {
			scrollheight -= i;
			if((i = screen->cur_row + n - 1 - bot) >= 0) {
				refreshheight -= i;
			}
		}
	}
	if(scrollheight > 0) {
		if (screen->incopy)
			CopyWait(screen);
		screen->incopy = -1;

		XMoveArea (VWindow(screen), screen->border, (scrolltop + n) *
		 FontHeight(screen) + screen->border + Titlebar(screen),
		 screen->border, scrolltop * FontHeight(screen) + screen->border
		 + Titlebar(screen), Width(screen), scrollheight *
		 FontHeight(screen));
	}
	if(refreshheight > 0)
		XTileSet (VWindow(screen), screen->border, refreshtop *
		 FontHeight(screen) + screen->border + Titlebar(screen),
		 Width(screen), refreshheight * FontHeight(screen),
		 screen->bgndtile);
    }
	/* adjust screen->buf */
	if(screen->sb && GetSaveState(screen->sb) && screen->cur_row == 0)
		ScrnDeleteLine(screen->allbuf, screen->bot_marg +
		 screen->savelines, 0, n, screen->max_col + 1);
	else
		ScrnDeleteLine(screen->buf, screen->bot_marg, screen->cur_row,
		 n, screen->max_col + 1);
}

/*
 * Insert n blanks at the cursor's position, no wraparound
 */
InsertChar (screen, n)
register Screen *screen;
register int n;
{
	register int width = n * FontWidth(screen), cx, cy;

	if(screen->cursor_state)
		HideCursor();
	screen->do_wrap = 0;
	if(screen->cur_row - screen->topline <= screen->max_row ||
	 screen->instatus) {
	    if(!AddToRefresh(screen)) {
		if(screen->scroll_amt)
			FlushScroll(screen);
	
		if (screen->incopy)
			CopyWait (screen);
		screen->incopy = -1;
	
		cx = CursorX (screen, screen->cur_col);
		cy = CursorY (screen, screen->cur_row);
		XMoveArea(VWindow(screen), cx, cy, cx + width, cy,
		     Width(screen) - (screen->cur_col + n) * FontWidth(screen),
		     FontHeight(screen));
		XPixSet(VWindow(screen), cx, cy,
		     width, FontHeight(screen), screen->instatus ?
		     screen->foreground : screen->background);
	
	    }
	}
	/* adjust screen->buf */
	ScrnInsertChar(screen->buf, screen->cur_row, screen->cur_col, n,
			screen->max_col + 1);
}

/*
 * Deletes n chars at the cursor's position, no wraparound.
 */
DeleteChar (screen, n)
register Screen *screen;
register int	n;
{
	register int width, cx, cy;

	if(screen->cursor_state)
		HideCursor();
	screen->do_wrap = 0;
	if (n > (width = screen->max_col + 1 - screen->cur_col))
	  	n = width;
		
	if(screen->cur_row - screen->topline <= screen->max_row ||
	 screen->instatus) {
	    if(!AddToRefresh(screen)) {
		if(screen->scroll_amt)
			FlushScroll(screen);
	
		width = n * FontWidth(screen);
	
		if (screen->incopy)
			CopyWait (screen);
		screen->incopy = -1;
	
		cx = CursorX (screen, screen->cur_col);
		cy = CursorY (screen, screen->cur_row);
		XMoveArea(VWindow(screen), cx + width, cy, cx, cy,
		     Width(screen) - (screen->cur_col + n) * FontWidth(screen),
		     FontHeight(screen));
		XPixSet (VWindow(screen),
		     screen->border + Width(screen) - width, cy,
		     width, FontHeight(screen), screen->instatus ?
		     screen->foreground : screen->background);
	
	    }
	}
	/* adjust screen->buf */
	ScrnDeleteChar (screen->buf, screen->cur_row, screen->cur_col, n,
			screen->max_col + 1);

}

/*
 * Clear from cursor position to beginning of display, inclusive.
 */
ClearAbove (screen)
register Screen *screen;
{
	register top, height;

	if(screen->cursor_state)
		HideCursor();
	if((top = -screen->topline) <= screen->max_row) {
		if(screen->scroll_amt)
			FlushScroll(screen);
		if((height = screen->cur_row + top) > screen->max_row)
			height = screen->max_row;
		if((height -= top) > 0)
			XTileSet(VWindow(screen), screen->border, top *
			 FontHeight(screen) + screen->border + Titlebar(screen),
			 Width(screen), height * FontHeight(screen),
			 screen->bgndtile);

		if(screen->cur_row - screen->topline <= screen->max_row)
			ClearLeft(screen);
	}
	ClearBufRows(screen, 0, screen->cur_row - 1);
}

/*
 * Clear from cursor position to end of display, inclusive.
 */
ClearBelow (screen)
register Screen *screen;
{
	register top;

	ClearRight(screen);
	if((top = screen->cur_row - screen->topline) <= screen->max_row) {
		if(screen->scroll_amt)
			FlushScroll(screen);
		if(++top <= screen->max_row)
			XTileSet(VWindow(screen), screen->border, top *
			 FontHeight(screen) + screen->border + Titlebar(screen),
			 Width(screen), (screen->max_row - top + 1) *
			 FontHeight(screen), screen->bgndtile);
	}
	ClearBufRows(screen, screen->cur_row + 1, screen->max_row);
}

/* 
 * Clear last part of cursor's line, inclusive.
 */
ClearRight (screen)
register Screen *screen;
{
	if(screen->cursor_state)
		HideCursor();
	screen->do_wrap = 0;
	if(screen->cur_row - screen->topline <= screen->max_row ||
	 screen->instatus) {
	    if(!AddToRefresh(screen)) {
		if(screen->scroll_amt)
			FlushScroll(screen);
		XPixSet(VWindow(screen),
		 CursorX(screen, screen->cur_col),
		 CursorY(screen, screen->cur_row),
		 Width(screen) - screen->cur_col * FontWidth(screen),
		 FontHeight(screen), screen->instatus ? screen->foreground :
		 screen->background);
	    }
	}
	bzero(screen->buf [2 * screen->cur_row] + screen->cur_col,
	       (screen->max_col - screen->cur_col + 1));
	bzero(screen->buf [2 * screen->cur_row + 1] + screen->cur_col,
	       (screen->max_col - screen->cur_col + 1));
}

/*
 * Clear first part of cursor's line, inclusive.
 */
ClearLeft (screen)
register Screen *screen;
{
	if(screen->cursor_state)
		HideCursor();
	screen->do_wrap = 0;
	if(screen->cur_row - screen->topline <= screen->max_row ||
	 screen->instatus) {
	    if(!AddToRefresh(screen)) {
		if(screen->scroll_amt)
			FlushScroll(screen);
	
		XPixSet (VWindow(screen),
		     screen->border, CursorY (screen, screen->cur_row),
		     (screen->cur_col + 1) * FontWidth(screen),
		     FontHeight(screen), screen->instatus ? screen->foreground :
		     screen->background);
	    }
	}
	bzero (screen->buf [2 * screen->cur_row], (screen->cur_col + 1));
	bzero (screen->buf [2 * screen->cur_row + 1], (screen->cur_col + 1));
}

/* 
 * Erase the cursor's line.
 */
ClearLine(screen)
register Screen *screen;
{
	if(screen->cursor_state)
		HideCursor();
	screen->do_wrap = 0;
	if(screen->cur_row - screen->topline <= screen->max_row ||
	 screen->instatus) {
	    if(!AddToRefresh(screen)) {
		if(screen->scroll_amt)
			FlushScroll(screen);
		XPixSet (VWindow(screen),
		     screen->border, CursorY (screen, screen->cur_row),
		     Width(screen), FontHeight(screen), screen->instatus ?
		     screen->foreground : screen->background);
	    }
	}
	bzero (screen->buf [2 * screen->cur_row], (screen->max_col + 1));
	bzero (screen->buf [2 * screen->cur_row + 1], (screen->max_col + 1));
}

ClearScreen(screen)
register Screen *screen;
{
	register int top;

	if(screen->cursor_state)
		HideCursor();
	screen->do_wrap = 0;
	if((top = -screen->topline) <= screen->max_row) {
		if(screen->scroll_amt)
			FlushScroll(screen);
		if(top == 0 && !screen->statusline)
			XClear(VWindow(screen));
		else
			XTileSet(VWindow(screen), screen->border, top *
			 FontHeight(screen) + screen->border + Titlebar(screen),
			 Width(screen), (screen->max_row - top + 1) *
			 FontHeight(screen), screen->bgndtile);
	}
	ClearBufRows (screen, 0, screen->max_row);
	screen->pagecnt = 0;
}

CopyWait(screen)
register Screen *screen;
{
	XEvent reply;
	XEvent *rep = &reply;

	while (1) {
		XWindowEvent (VWindow(screen), ExposeRegion|ExposeCopy, &reply);
		switch (reply.type) {
		case ExposeRegion:
			if (((XExposeEvent *)rep)->detail == ExposeCopy &&
			    screen->incopy <= 0) {
				screen->incopy = 1;
				if (screen->scrolls > 0)
					screen->scrolls--;
			}
			HandleExposure (screen, &reply);
			break;
		case ExposeCopy:
			if (screen->incopy <= 0 && screen->scrolls > 0)
				screen->scrolls--;
			if (screen->scrolls == 0) {
				screen->incopy = 0;
				return;
			}
			screen->incopy = -1;
			break;
		}
	}
}
/*
 * This routine handles exposure events
 */
HandleExposure (screen, reply)
register Screen *screen;
register XExposeEvent *reply;
{
	register int toprow, leftcol, nrows, ncols;
	extern Terminal term;	/* kludge */
	XExposeRegionEvent event;

	if((toprow = (reply->y - screen->border - Titlebar(screen)) /
	 FontHeight(screen)) < 0)
		toprow = 0;
	if((leftcol = (reply->x - screen->border) / FontWidth(screen)) < 0)
		leftcol = 0;
	nrows = (reply->y + reply->height - 1 - screen->border
	 - Titlebar(screen)) / FontHeight(screen) - toprow + 1;
	ncols = (reply->x + reply->width - 1 - screen->border) /
			FontWidth(screen) - leftcol + 1;
	toprow -= screen->scrolls;
	if (toprow < 0) {
		nrows += toprow;
		toprow = 0;
	}
	if (toprow + nrows - 1 > screen->max_row)
		nrows = screen->max_row - toprow + 1 + screen->statusline;
	if (leftcol + ncols - 1 > screen->max_col)
		ncols = screen->max_col - leftcol + 1;

	if (nrows > 0 && ncols > 0) {
		ScrnRefresh (screen, toprow, leftcol, nrows, ncols);
		if (screen->cur_row >= toprow &&
		    screen->cur_row < toprow + nrows &&
		    screen->cur_col >= leftcol &&
		    screen->cur_col < leftcol + ncols)
			return (1);
	}
	return (0);
}

ReverseVideo (term)
	Terminal *term;
{
	register Screen *screen = &term->screen;
	register Pixmap pix;
	register int tmp;
	register Window tek = TWindow(screen);
	extern Pixmap B_Pixmap;
	extern Pixmap W_Pixmap;

	if(screen->color & C_BACKGROUND)
		XFreePixmap(screen->bgndtile);
	tmp = screen->background;
	if(screen->cursorcolor == screen->foreground)
		screen->cursorcolor = tmp;
	if(screen->mousecolor == screen->foreground)
		screen->mousecolor = tmp;
	screen->background = screen->foreground;
	screen->foreground = tmp;

	screen->color = (screen->color & ~C_FBMASK) | switchfb[screen->color
	 & C_FBMASK];

	if(screen->color & C_BACKGROUND) {
		if(!(screen->bgndtile = XMakeTile(screen->background)))
			Error(ERROR_UBACK);
	} else
		screen->bgndtile = (screen->background == W_Pixel) ? W_Pixmap
		 : B_Pixmap;

	/* X11 arrow cursor feature */
	XFreeCursor(screen->curs);
	if (curs_shape && strcmp(curs_shape, "arrow") == 0)
		screen->curs = make_arrow(screen->mousecolor,
		    screen->background, GXcopy);
	else
		screen->curs = make_xterm(screen->mousecolor,
		    screen->background, GXcopy);
	XFreeCursor(screen->arrow);
	screen->arrow = make_arrow(screen->mousecolor, screen->background,
	 GXcopy);

	XDefineCursor(VWindow(screen), screen->curs);
	if(screen->sb)
		XDefineCursor(screen->sb->bar, screen->sb->cursor =
		 screen->arrow);
	if(screen->title.tbar)
		XDefineCursor(screen->title.tbar, screen->arrow);
	if(tek)
		XDefineCursor(tek, screen->arrow);
#ifdef MODEMENU
	MenuNewCursor(screen->arrow);
#endif MODEMENU

	if (screen->background < 2 && screen->foreground < 2) {
	    if (screen->bgndtile == B_Pixmap)
		screen->bordertile = W_Pixmap;
	    else if (screen->bgndtile == W_Pixmap)
		screen->bordertile = B_Pixmap;
	    pix = screen->bordertile;
	    if(screen->sb)
		XChangeBorder (screen->sb->bar, pix);
	    if(screen->title.tbar)
		XChangeBorder (screen->title.tbar, pix);
	    if(tek && screen->Ttitle.tbar)
		XChangeBorder (screen->Ttitle.tbar, pix);
	    if(screen->borderwidth > 0) {
		XChangeBorder (VWindow(screen), pix);
		XChangeBorder (screen->iconVwin.window, pix);
		if(tek) {
		    XChangeBorder (tek, pix);
		    XChangeBorder (screen->iconTwin.window, pix);
		}
	    }
	}

	XChangeBackground (VWindow(screen), screen->bgndtile);
	XChangeBackground (screen->iconVwin.window, screen->bgndtile);
	if(screen->title.tbar)
	    XChangeBackground (screen->title.tbar, screen->bgndtile);
	if(tek) {
	    XChangeBackground (screen->iconTwin.window, screen->bgndtile);
	    if(screen->Ttitle.tbar)
		XChangeBackground (screen->Ttitle.tbar, screen->bgndtile);
	    TekReverseVideo(screen);
	}
	XClear (VWindow(screen));
	XClear (screen->iconVwin.window);
	ScrnRefresh (screen, 0, 0, screen->max_row + 1 + screen->statusline,
	 screen->max_col + 1);
	if(screen->Tshow) {
	    XClear (tek);
	    XClear (screen->iconTwin.window);
	    TekExpose((XExposeWindowEvent *)0);
	}
	if(Titlebar(screen)) {
	    XClear(screen->title.tbar);
	    VTTitleExpose(NULL);
	    if(screen->Tshow) {
		XClear(screen->Ttitle.tbar);
		TekTitleExpose(NULL);
	    }
	}
}
