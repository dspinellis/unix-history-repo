#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

/* util.c */

#ifndef lint
static char *rcsid_util_c = "$Header: util.c,v 10.11 86/04/28 15:05:29 jg Exp $";
#endif	lint

#include <stdio.h>
#include <X/Xlib.h>
#include <errno.h>
#include <signal.h>

#include "ptyx.h"

extern errno;
extern debug;
#ifdef JUMPSCROLL
/*
 * These routines are used for the jump scroll feature
 */
FlushScroll(screen)
register Screen *screen;
{
	register int height = screen->bot_marg - screen->top_marg + 1;
	register int samount = screen->scroll_amt;
	register int ramount = screen->refresh_amt;
	register int scrolled = samount * screen->f_height;
	register int refreshed = ramount * screen->f_height;
	register int top = screen->top_marg * screen->f_height + screen->border;
	int rtop;

  if(samount > 0) {
    rtop = screen->bot_marg - ramount + 1;
  } else {
    rtop = screen->top_marg;
    top -= scrolled;
    ramount = -ramount;
    refreshed = -refreshed;
  }

  if (ramount != height) {

      if (screen->multiscroll && samount == 1 &&
	  screen->top_marg == 0 && screen->bot_marg == screen->max_row) {
	      if (screen->incopy < 0 && screen->scrolls == 0)
		      CopyWait (screen);
	      screen->scrolls++;
      } else {
	      if (screen->incopy)
		      CopyWait (screen);
	      screen->incopy = -1;
      }

      XMoveArea (screen->window,
		 screen->border, top + scrolled, screen->border, top,
		 screen->width, height * screen->f_height - refreshed);
  }
  screen->scroll_amt = 0;
  screen->refresh_amt = 0;
  XTileSet (screen->window, screen->border,
	    rtop * screen->f_height + screen->border,
	    screen->width, refreshed, screen->bgndtile);
  ScrnRefresh(screen, rtop, 0, ramount, screen->max_col + 1);
}

AddToRefresh(screen)
register Screen *screen;
{
	register int amount = screen->refresh_amt;
	register int row = screen->cur_row;

	if(amount == 0)
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
#endif JUMPSCROLL

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
	register int height = screen->bot_marg - screen->top_marg + 1;
	register int covered;
	register int border = screen->border;
	register int top = screen->top_marg * screen->f_height + border;

	if (amount > height)
		amount = height;
#ifdef JUMPSCROLL
    if(screen->jumpscroll) {
	if(screen->scroll_amt > 0) {
		if(screen->refresh_amt + amount > height)
			FlushScroll(screen);
		screen->scroll_amt += amount;
		screen->refresh_amt += amount;
	} else {
		if(screen->scroll_amt < 0)
			FlushScroll(screen);
		screen->scroll_amt = amount;
		screen->refresh_amt = amount;
	}
    } else {
#endif JUMPSCROLL

	if (amount == height) {
		ClearScreen(screen);
		return;
	}

	covered = amount * screen->f_height;

	if (screen->multiscroll
	&& amount==1
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

	XMoveArea(screen->window, border, top + covered, border, top,
		screen->width, height * screen->f_height - covered);
	XTileSet(screen->window, border,
		(screen->bot_marg - amount + 1) * screen->f_height + border,
		screen->width, covered, screen->bgndtile);
#ifdef JUMPSCROLL
	}
#endif JUMPSCROLL

	ScrnDeleteLine(screen->buf, screen->bot_marg, screen->top_marg,
			amount, screen->max_col + 1);
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
	register int height = screen->bot_marg - screen->top_marg + 1;
	register int border = screen->border;
	register int top = screen->top_marg * screen->f_height + border;
	register int covered;

	amount = (amount < height) ? amount : height;
#ifdef JUMPSCROLL
    if(screen->jumpscroll) {
	if(screen->scroll_amt < 0) {
		if(-screen->refresh_amt + amount > height)
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
#endif JUMPSCROLL
	covered = amount * screen->f_height;

	if (screen->incopy)
		CopyWait (screen);
	screen->incopy = -1;

	XMoveArea (screen->window,
		border, top,
		border, top + covered,
		screen->width, height * screen->f_height - covered);
	XTileSet (screen->window,
		border, top,
		screen->width, covered,
		screen->bgndtile);

#ifdef JUMPSCROLL
	}
#endif JUMPSCROLL
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
	register int height;
	register int bottom = screen->bot_marg;

	if (screen->cur_row < screen->top_marg ||
	    screen->cur_row > screen->bot_marg) return;
	
	if (screen->cur_row + n - 1 > bottom) n = bottom - screen->cur_row + 1;
#ifdef JUMPSCROLL
    if(screen->jumpscroll) {
	if(screen->scroll_amt <= 0&&screen->cur_row <= -screen->refresh_amt) {
		if(-screen->refresh_amt + n > height)
			FlushScroll(screen);
		screen->scroll_amt -= n;
		screen->refresh_amt -= n;
	} else if(screen->scroll_amt)
		FlushScroll(screen);
    }
    if(!screen->scroll_amt) {
#endif JUMPSCROLL


	height = n * screen->f_height;

	screen->do_wrap = 0;

	if (screen->incopy)
		CopyWait (screen);
	screen->incopy = -1;

	/*
	 * move stuff down.
	 * clear hole.
	 */
	XMoveArea(screen->window,
		     screen->border, CursorY (screen),
		     screen->border, CursorY (screen) + height,
		     screen->width,
		     (bottom + 1 - screen->cur_row - n) * screen->f_height);
	XTileSet(screen->window,
		screen->border, CursorY (screen),
		screen->width, height, screen->bgndtile);

#ifdef JUMPSCROLL
	}
#endif JUMPSCROLL
	/* adjust screen->buf */
	ScrnInsertLine(screen->buf, bottom, screen->cur_row, n,
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
	register int height;
	register int bottom = screen->bot_marg;

	if (screen->cur_row < screen->top_marg
	|| screen->cur_row > screen->bot_marg)
		return;
	
	if (screen->cur_row + n - 1 > bottom)
		n = bottom - screen->cur_row + 1;
#ifdef JUMPSCROLL
    if(screen->jumpscroll) {
	if(screen->scroll_amt >= 0 && screen->cur_row == screen->top_marg) {
		if(screen->refresh_amt + n > height)
			FlushScroll(screen);
		screen->scroll_amt += n;
		screen->refresh_amt += n;
	} else if(screen->scroll_amt)
		FlushScroll(screen);
    }
    if(!screen->scroll_amt) {
#endif JUMPSCROLL

	height = n * screen->f_height;
	screen->do_wrap = 0;

	if (screen->incopy)
		CopyWait(screen);
	screen->incopy = -1;

	/*
	 * move stuff up.
	 * clear bottom.
	 */
	XMoveArea(screen->window,
		     screen->border, CursorY (screen) + height,
		     screen->border, CursorY (screen),
		     screen->width,
		     (bottom + 1 - screen->cur_row - n) * screen->f_height);
	XTileSet(screen->window,
		     screen->border,
		     (bottom + 1 - n) * screen->f_height + screen->border,
		     screen->width, height, screen->bgndtile);

#ifdef JUMPSCROLL
	}
#endif JUMPSCROLL
	/* adjust screen->buf */
	ScrnDeleteLine(screen->buf, bottom, screen->cur_row, n,
			screen->max_col + 1);
}

/*
 * Insert n blanks at the cursor's position, no wraparound.
 */
InsertChar (screen, n)
register Screen *screen;
register int n;
{
	register int width = n * screen->f_width;

#ifdef JUMPSCROLL
    if(!AddToRefresh(screen)) {
	if(screen->scroll_amt)
		FlushScroll(screen);
#endif JUMPSCROLL

	screen->do_wrap = 0;

	if (screen->incopy)
		CopyWait (screen);
	screen->incopy = -1;

	XMoveArea(screen->window,
		     CursorX (screen), CursorY (screen),
		     CursorX (screen) + width, CursorY (screen),
		     screen->width - (screen->cur_col + n) * screen->f_width,
		     screen->f_height);
	XTileSet(screen->window,
		     CursorX (screen), CursorY (screen),
		     width, screen->f_height, screen->bgndtile);

#ifdef JUMPSCROLL
	}
#endif JUMPSCROLL
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
	register int width; 

	if (screen->cur_col + n > screen->max_col + 1)
	  	n = screen->max_col + 1 - screen->cur_col;

#ifdef JUMPSCROLL
    if(!AddToRefresh(screen)) {
	if(screen->scroll_amt)
		FlushScroll(screen);
#endif JUMPSCROLL

	width = n * screen->f_width;

	screen->do_wrap = 0;

	if (screen->incopy)
		CopyWait (screen);
	screen->incopy = -1;

	XMoveArea(screen->window,
		     CursorX (screen) + width, CursorY (screen),
		     CursorX (screen), CursorY (screen),
		     screen->width - (screen->cur_col + n) * screen->f_width,
		     screen->f_height);
	XTileSet (screen->window,
		     screen->border + screen->width - width, CursorY (screen),
		     width, screen->f_height, screen->bgndtile);

#ifdef JUMPSCROLL
	}
#endif JUMPSCROLL
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
#ifdef JUMPSCROLL
	if(screen->scroll_amt)
		FlushScroll(screen);
#endif JUMPSCROLL
	ClearLeft(screen);
	
	XTileSet(screen->window, screen->border, 0,
		screen->width, CursorY (screen), screen->bgndtile);
	ClearBufRows(screen, 0, screen->cur_row - 1);
}

/*
 * Clear from cursor position to end of display, inclusive.
 */
ClearBelow (screen)
register Screen *screen;
{
	register int sy = CursorY (screen) + screen->f_height;

#ifdef JUMPSCROLL
	if(screen->scroll_amt)
		FlushScroll(screen);
#endif JUMPSCROLL
	ClearRight(screen);
	XTileSet(screen->window, screen->border, sy,
	     screen->width, screen->height - sy + screen->border, 
	     screen->bgndtile);
	ClearBufRows(screen, screen->cur_row + 1, screen->max_row);
}

/* 
 * Clear last part of cursor's line, inclusive.
 */
ClearRight (screen)
register Screen *screen;
{
	screen->do_wrap = 0;

#ifdef JUMPSCROLL
    if(!AddToRefresh(screen)) {
	if(screen->scroll_amt)
		FlushScroll(screen);
#endif JUMPSCROLL
	XTileSet(screen->window,
	     CursorX (screen), CursorY (screen),
	     screen->width-screen->cur_col * screen->f_width,screen->f_height,
	     screen->bgndtile);
#ifdef JUMPSCROLL
    }
#endif JUMPSCROLL
	bzero((char *)(screen->buf [screen->cur_row] + screen->cur_col),
	       sizeof (short) * (screen->max_col - screen->cur_col + 1));
}

/*
 * Clear first part of cursor's line, inclusive.
 */
ClearLeft (screen)
register Screen *screen;
{
	screen->do_wrap = 0;

#ifdef JUMPSCROLL
    if(!AddToRefresh(screen)) {
	if(screen->scroll_amt)
		FlushScroll(screen);
#endif JUMPSCROLL

	XTileSet (screen->window,
	     screen->border, CursorY (screen),
	     (screen->cur_col + 1) * screen->f_width,
	     screen->f_height, screen->bgndtile);
#ifdef JUMPSCROLL
    }
#endif JUMPSCROLL
	bzero ((char *)screen->buf [screen->cur_row],
		sizeof (short) * (screen->cur_col + 1));
}

/* 
 * Erase the cursor's line.
 */
ClearLine(screen)
register Screen *screen;
{
	screen->do_wrap = 0;

#ifdef JUMPSCROLL
    if(!AddToRefresh(screen)) {
	if(screen->scroll_amt)
		FlushScroll(screen);
#endif JUMPSCROLL
	XTileSet (screen->window,
	     screen->border, CursorY (screen),
	     screen->width, screen->f_height, screen->bgndtile);
#ifdef JUMPSCROLL
    }
#endif JUMPSCROLL
	bzero ((char *)screen->buf [screen->cur_row],
		sizeof (short) * (screen->max_col + 1));
}

ClearScreen(screen)
register Screen *screen;
{
	screen->do_wrap = 0;
	
#ifdef JUMPSCROLL
	if(screen->scroll_amt)
		FlushScroll(screen);
#endif JUMPSCROLL
	XClear(screen->window);
	ClearBufRows (screen, 0, screen->max_row);
}

CopyWait(screen)
register Screen *screen;
{
	XEvent reply;
	XEvent *rep = &reply;

	while (1) {
		XWindowEvent (screen->window, ExposeRegion|ExposeCopy, &reply);
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
	int toprow, leftcol, nrows, ncols;
	extern Terminal term;	/* kludge */
	XExposeRegionEvent event;

	toprow = (reply->y - screen->border) / screen->f_height;
	leftcol = (reply->x - screen->border) / screen->f_width;
	nrows = (reply->y + reply->height - 1 - screen->border) /
			screen->f_height - toprow + 1;
	ncols = (reply->x + reply->width - 1 - screen->border) /
			screen->f_width - leftcol + 1;
	toprow -= screen->scrolls;
	if (toprow < 0) {
		nrows += toprow;
		toprow = 0;
	}
	if (toprow + nrows - 1 > screen->max_row)
		nrows = screen->max_row - toprow + 1;
	if (leftcol + ncols - 1 > screen->max_col)
		ncols = screen->max_col - leftcol + 1;

	if (nrows > 0 && ncols > 0) {
		if (screen->TekEmu && reply->detail != ExposeCopy)
			/* Clear to avoid possible dangling cursor */
			XTileSet (screen->window,
			     leftcol * screen->f_width + screen->border,
			     toprow * screen->f_height + screen->border,
			     ncols * screen->f_width,
			     nrows * screen->f_height,
			     screen->bgndtile);
		ScrnRefresh (screen, toprow, leftcol, nrows, ncols);
		/* only do the tek refresh on the last refresh event */
		if (screen->TekEmu) {
			XSync(0);	/* make sure they are all here */
			if (XPending() != 0) {
				XPeekEvent(&event);
				if (event.type != ExposeRegion) {
					if (reply->detail != ExposeCopy)
						TekRefresh(&term);
				}
			}
			else {
				if (reply->detail != ExposeCopy)
					TekRefresh (&term);
			}
		}
		if (screen->cur_row >= toprow &&
		    screen->cur_row < toprow + nrows &&
		    screen->cur_col >= leftcol &&
		    screen->cur_col < leftcol + ncols)
			return (1);
	}
	return (0);
}

Panic(s, a)
char	*s;
{
	fprintf(stderr, "PANIC!	");
	fprintf(stderr, s, a);
	fprintf(stderr, "\r\n");
	fflush(stderr);
}

Error ()
{
	fprintf (stderr, "Error %d: ", errno);
	perror ("");
	Cleanup(66);
}

/*
 * cleanup by sending SIGHUP to client processes
 */
Cleanup (code)
int code;
{
	extern Terminal term;
	register Screen *screen;
	register long pgrp;
	screen = &term.screen;
	if (screen->pid > 1) {
		pgrp = getpgrp(screen->pid);
		if (pgrp > 1) killpg(pgrp, SIGHUP);
	}
	exit(code);
}

/*
 * sets the value of var to be arg in the Unix 4.2 BSD environment env.
 * Var should end with '=' (bindings are of the form "var=value").
 * This procedure assumes the memory for the first level of environ
 * was allocated using malloc.
 */
Setenv (var, value)
register char *var, *value;
{
	extern char **environ;
	register int index = 0;

	while (environ [index] != NULL) {
	    if (strncmp (environ [index], var, strlen (var)) == 0) {
		/* found it */
		environ [index] = (char *) malloc (strlen (var) + strlen (value));
		strcpy (environ [index], var);
		strcat (environ [index], value);
		return;
	    }
	    index ++;
	}

	if (debug) printf ("expanding env\n");
	environ = (char **) realloc((char *)environ, sizeof(char *) * (index+2));
	if (environ == NULL) {
	    fprintf (stderr, "Setenv: malloc out of memory\n");
	    exit (1);
	}

	environ [index] = (char *) malloc (strlen (var) + strlen (value));
	strcpy (environ [index], var);
	strcat (environ [index], value);
	environ [++index] = NULL;
}

/*
 * returns a pointer to the first occurrence of s2 in s1,
 * or NULL if there are none.
 */
char *strindex (s1, s2)
register char	*s1, *s2;
{
	register char	*s3;
	char		*index();

	while ((s3=index(s1, *s2)) != NULL) {
		if (strncmp(s3, s2, strlen(s2)) == 0)
			return (s3);
		s1 = ++s3;
	}
	return (NULL);
}
