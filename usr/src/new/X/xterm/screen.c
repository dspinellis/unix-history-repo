/*
 *	$Source: /u1/X/xterm/RCS/screen.c,v $
 *	$Header: screen.c,v 10.100 86/12/01 14:45:17 jg Rel $
 */

#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

/* screen.c */

#ifndef lint
static char csrg_id[] = "@(#)screen.c	1.6\t(Berkeley/CSRG)\t9/21/87";
static char sccs_id[] = "@(#)screen.c\tX10/6.6B\t12/26/86";
#endif	lint

#include <X/Xlib.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <signal.h>
#include "scrollbar.h"
#include "ptyx.h"
#include "error.h"

extern char *calloc();
extern char *malloc();
extern char *realloc();

ScrnBuf Allocate (nrow, ncol)
/*
   allocates memory for a 2-dimensional array of chars and returns a pointer
   thereto
   each line is formed from a pair of char arrays.  The first (even) one is
   the actual character array and the second (odd) one is the attributes.
 */
register int nrow, ncol;
{
	register ScrnBuf base;

	if ((base = (ScrnBuf) calloc ((nrow *= 2), sizeof (char *))) == 0)
		SysError (ERROR_SCALLOC);

	for (nrow--; nrow >= 0; nrow--)
		if ((base [nrow] = calloc (ncol, sizeof(char))) == 0)
			SysError (ERROR_SCALLOC2);

	return (base);
}

ScreenWrite (screen, str, flags, length)
/*
   Writes str into buf at row row and column col.  Characters are set to match
   flags.
 */
Screen *screen;
char *str;
register unsigned flags;
register int length;		/* length of string */
{
	register char *att;
	register int avail  = screen->max_col - screen->cur_col + 1;
	register char *col;

	if (length > avail)
	    length = avail;
	if (length <= 0)
		return;

	col = screen->buf[avail = 2 * screen->cur_row] + screen->cur_col;
	att = screen->buf[avail + 1] + screen->cur_col;
	flags &= ATTRIBUTES;
	bcopy(str, col, length);
	while(length-- > 0)
		*att++ = flags;
}

ScrnInsertLine (sb, last, where, n, size)
/*
   Inserts n blank lines at sb + where, treating last as a bottom margin.
   Size is the size of each entry in sb.
   Requires: 0 <= where < where + n <= last
   	     n <= MAX_ROWS
 */
register ScrnBuf sb;
int last;
register int where, n, size;
{
	register int i;
	char *save [2 * MAX_ROWS];

	/* save n lines at bottom */
	bcopy ((char *) &sb [2 * (last -= n - 1)], (char *) save,
		2 * sizeof (char *) * n);
	
	/* clear contents of old rows */
	for (i = 2 * n - 1; i >= 0; i--)
		bzero ((char *) save [i], size);

	/* move down lines */
	bcopy ((char *) &sb [2 * where], (char *) &sb [2 * (where + n)],
		2 * sizeof (char *) * (last - where));

	/* reuse storage for new lines at where */
	bcopy ((char *)save, (char *) &sb[2 * where], 2 * sizeof(char *) * n);
}


ScrnDeleteLine (sb, last, where, n, size)
/*
   Deletes n lines at sb + where, treating last as a bottom margin.
   Size is the size of each entry in sb.
   Requires 0 <= where < where + n < = last
   	    n <= MAX_ROWS
 */
register ScrnBuf sb;
register int n, last, size;
int where;
{
	register int i;
	char *save [2 * MAX_ROWS];

	/* save n lines at where */
	bcopy ((char *) &sb[2 * where], (char *)save, 2 * sizeof(char *) * n);

	/* clear contents of old rows */
	for (i = 2 * n - 1 ; i >= 0 ; i--)
		bzero ((char *) save [i], size);

	/* move up lines */
	bcopy ((char *) &sb[2 * (where + n)], (char *) &sb[2 * where],
		2 * sizeof (char *) * ((last -= n - 1) - where));

	/* reuse storage for new bottom lines */
	bcopy ((char *)save, (char *) &sb[2 * last],
		2 * sizeof(char *) * n);
}


ScrnInsertChar (sb, row, col, n, size)
/*
   Inserts n blanks in sb at row, col.  Size is the size of each row.
 */
ScrnBuf sb;
int row, size;
register int col, n;
{
	register int i, j;
	register char *ptr = sb [2 * row];
	register char *att = sb [2 * row + 1];

	for (i = size - 1; i >= col + n; i--) {
		ptr[i] = ptr[j = i - n];
		att[i] = att[j];
	}

	bzero (ptr + col, n);
	bzero (att + col, n);
}


ScrnDeleteChar (sb, row, col, n, size)
/*
   Deletes n characters in sb at row, col. Size is the size of each row.
 */
ScrnBuf sb;
register int row, size;
register int n, col;
{
	register char *ptr = sb[2 * row];
	register char *att = sb[2 * row + 1];
	register nbytes = (size - n - col);

	bcopy (ptr + col + n, ptr + col, nbytes);
	bcopy (att + col + n, att + col, nbytes);
	bzero (ptr + size - n, n);
	bzero (att + size - n, n);
}


ScrnRefresh (screen, toprow, leftcol, nrows, ncols)
/*
   Repaints the area enclosed by the parameters.
   Requires: (toprow, leftcol), (toprow + nrows, leftcol + ncols) are
   	     coordinates of characters in screen;
	     nrows and ncols positive.
 */
register Screen *screen;
int toprow, leftcol, nrows, ncols;
{
	int y = toprow * FontHeight(screen) + screen->border + Titlebar(screen);
	register int row;
	register int topline = screen->topline;
	int maxrow = toprow + nrows - 1;
	int scrollamt = screen->scroll_amt;
	int max = screen->max_row;
	int dostatus = 0, left, width;
	

	if(screen->statusline && maxrow == screen->max_row + 1) {
		dostatus++;
		maxrow--;
	}
	if(screen->cursor_col >= leftcol && screen->cursor_col <=
	 (leftcol + ncols - 1) && screen->cursor_row >= toprow + topline &&
	 screen->cursor_row <= maxrow + topline)
		screen->cursor_state = OFF;
    for( ; ; ) {
	for (row = toprow; row <= maxrow; y += FontHeight(screen), row++)
	{
	   register char *chars;
	   register char *att;
	   register int col = leftcol;
	   int maxcol = leftcol + ncols - 1;
	   int lastind;
	   int flags;
	   int gxfunction;
	   Font fnt;
	   int x, yb, pix, n;

	   lastind = row - scrollamt;
	   if (lastind < 0 || lastind > max)
	   	continue;
	   chars = screen->buf [2 * (lastind + topline)];
	   att = screen->buf [2 * (lastind + topline) + 1];

	   while (col <= maxcol && (att[col] & ~BOLD) == 0 &&
	    (chars[col] & ~040) == 0)
		col++;

	   while (col <= maxcol && (att[maxcol] & ~BOLD) == 0 &&
	    (chars[maxcol] & ~040) == 0)
		maxcol--;

	   if (col > maxcol) continue;

	   flags = att[col];

	   fnt = ActiveIcon(screen)
		 ? screen->fnt_icon
		 : (flags & BOLD)
		   ? screen->fnt_bold
		   : screen->fnt_norm;

	   x = col * FontWidth(screen) + screen->border;
	   lastind = col;

	   for (; col <= maxcol; col++) {
		if (att[col] != flags) {
		   if (((flags & INVERSE) != 0) ^ (dostatus < 0 &&
		    screen->reversestatus))
		   	XText (VWindow(screen), x, y, &chars[lastind],
				n = col - lastind, fnt,
		   		pix = screen->background, screen->foreground);
		   else
		 	XText (VWindow(screen), x, y, &chars[lastind],
				n = col - lastind, fnt,
		   		pix = screen->foreground, screen->background);
		   if((flags & BOLD) && screen->enbolden)
		 	XTextMask (VWindow(screen), x + 1, y, &chars[lastind],
				n, fnt, pix);
		   if(flags & UNDERLINE) {
			yb = y + FontHeight(screen) - 2;
			XLine(VWindow(screen), x, yb, x + n * FontWidth(screen),
			 yb, 1, 1, pix, GXcopy, AllPlanes);
		   }

		   x += (col - lastind) * FontWidth(screen);

		   lastind = col;

		   flags = att[col];

		   fnt = ActiveIcon(screen)
		   	 ? screen->fnt_icon
			 : (flags & BOLD)
			   ? screen->fnt_bold
			   : screen->fnt_norm;
		}

		if(chars[col] == 0)
			chars[col] = ' ';
	   }

	   if (((flags & INVERSE) != 0) ^ (dostatus < 0 &&
	    screen->reversestatus))
	   	XText (VWindow(screen), x, y, &chars[lastind],
		 n = col - lastind, fnt, pix = screen->background,
		 screen->foreground);
	   else
	   	XText (VWindow(screen), x, y, &chars[lastind],
		 n = col - lastind, fnt, pix = screen->foreground,
		 screen->background);
	   if((flags & BOLD) && screen->enbolden)
		XTextMask (VWindow(screen), x + 1, y, &chars[lastind],
			n, fnt, pix);
	   if(flags & UNDERLINE) {
		yb = y + FontHeight(screen) - 2;
		XLine(VWindow(screen), x, yb, x + n * FontWidth(screen), yb, 1, 1,
		 pix, GXcopy, AllPlanes);
	   }
	}
	if(dostatus <= 0)
		break;
	dostatus = -1;
	topline = 0;
	scrollamt = 0;
	toprow = maxrow = max = screen->max_row + 1;
	left = leftcol * FontWidth(screen) + screen->border;
	width = ncols * FontWidth(screen);
	if(leftcol == 0) {
		left--;
		width++;
	}
	if(leftcol + ncols - 1 >= screen->max_col)
		width++;
	XPixSet(VWindow(screen), left, y, width, screen->statusheight,
	 screen->reversestatus ? screen->foreground : screen->background);
	if(!screen->reversestatus)
		StatusBox(screen);
	y++;
    }
}

ClearBufRows (screen, first, last)
/*
   Sets the rows first though last of the buffer of screen to spaces.
   Requires first <= last; first, last are rows of screen->buf.
 */
register Screen *screen;
register int first, last;
{
	first *= 2;
	last = 2 * last + 1;
	while (first <= last)
		bzero (screen->buf [first++], (screen->max_col + 1));
}

ScreenResize (screen, width, height, flags)
/*
   Resizes screen:
   1. If new window would have fractional characters, sets window size so as to
      discard fractional characters and returns -1.
      Minimum screen size is 1 X 1.
      Note that this causes another ExposeWindow event.
   2. Enlarges screen->buf if necessary.  New space is appended to the bottom
      and to the right
   3. Reduces  screen->buf if necessary.  Old space is removed from the bottom
      and from the right
   4. Cursor is positioned as closely to its former position as possible
   5. Sets screen->max_row and screen->max_col to reflect new size
   6. Maintains the inner border.
   7. Clears origin mode and sets scrolling region to be entire screen.
   8. Returns 0
 */
register Screen *screen;
int width, height;
unsigned *flags;
{
	register int rows, cols;
	register int index;
	register int savelines;
	register ScrnBuf sb = screen->allbuf;
	register ScrnBuf ab = screen->altbuf;
	register int x;
	register int border = 2 * screen->border;
	register int extra, i, j, k;
	register char *sl0, *sl1;	/* keep status line */
	double scale_x, scale_y;
#ifdef TIOCSWINSZ
	struct winsize ws;
#endif TIOCSWINSZ


	/* don't resize on icon exposure */
	if (ActiveIcon(screen))
		return( 0 );

	extra = Titlebar(screen) + screen->statusheight;
	/* round so that it is unlikely the screen will change size on  */
	/* small mouse movements.					*/
	rows = (height + FontHeight(screen) / 2 - border - extra) /
	 FontHeight(screen);
	cols = (width + FontWidth(screen) / 2 - border - screen->scrollbar) /
	 FontWidth(screen);
	if (rows < 1) rows = 1;
	if (cols < 1) cols = 1;

	if ((width - border - screen->scrollbar) % FontWidth(screen)
	 != 0 || (height - border - extra) % FontHeight(screen) != 0 ||
	 rows < screen->minrows) {
		XChangeWindow (VWindow(screen),
		 cols * FontWidth(screen) + border + screen->scrollbar,
		 (rows < screen->minrows ? screen->minrows : rows) *
		 FontHeight(screen) + border + extra);
		return (-1);
	}

	/* change buffers if the screen has changed size */
	if (screen->max_row != rows - 1 || screen->max_col != cols - 1) {
		if(screen->cursor_state)
			HideCursor();
		savelines = screen->sb ? screen->savelines : 0;
		j = screen->max_col + 1;
		i = cols - j;
		k = screen->max_row;
		if(ab) {
			/* resize current lines in alternate buf */
			for (index = x = 0; index <= k; x += 2, index++) {
				if ((ab[x] = realloc(ab[x], cols)) == NULL)
					SysError(ERROR_SREALLOC);
				if((ab[x + 1] = realloc(ab[x + 1], cols)) ==
				 NULL)
					SysError (ERROR_SREALLOC2);
				if (cols > j) {
					bzero (ab [x] + j, i);
					bzero (ab [x + 1] + j, i);
				}
			}
		}
		/* resize current lines */
		k += savelines + 1;	/* includes status line */
		for (index = x = 0; index <= k; x += 2, index++) {
			if ((sb[x] = realloc(sb[x], cols)) == NULL)
				SysError(ERROR_SREALLOC3);
			if((sb[x + 1] = realloc(sb[x + 1], cols)) == NULL)
				SysError (ERROR_SREALLOC4);
			if (cols > j) {
				bzero (sb [x] + j, i);
				bzero (sb [x + 1] + j, i);
			}
		}

		/*
		 * Adjust the number of lines, either discarding the
		 * oldest (if we're getting shorter) or padding at
		 * the "old" end with blanks (if we're getting taller.
		 *
		 * Throughout this section, i is the number of lines
		 * we need, j is the number we currently have & index
		 * is the difference.
		 */
		i = rows + savelines + 1;
		j = screen->max_row + savelines + 2;
		index = j - i;
		if (index > 0) {
			/* we're getting shorter - free the oldest lines */
			for (x = 0; x < 2*index; ) {
				free (sb[x++]);
			}
			/* move the newer stuff into the right place */
			k = i * 2 * sizeof(*sb);
			bcopy (&sb[2*index], sb, k);
			sb = (ScrnBuf) realloc (sb, k);
			if (! sb)
				SysError (ERROR_RESIZE2);

			/* do the same thing for the alternate buf */
			if (ab) {
				for (x = 0; x < 2*index;) {
					free (ab[x++]);
				}
				k = (screen->max_row + 1)* sizeof(*sb);
				bcopy (&ab[2*index], ab, k);
				ab = (ScrnBuf) realloc (ab, k);
				if (! ab)
					SysError (ERROR_RESIZE);
			}
		}
		else if (index < 0) {
			/* we're getting taller - add 'index' lines */
			/* at the "old" end of the saved lines */
			sb = (ScrnBuf) realloc (sb, i * 2 * sizeof(*sb));
			if (! sb)
				SysError (ERROR_RESIZE2);

			/* move up the stuff we want at the new end */
			/* of the buffer and fill the hole with blanks */
			k = -2 * index;
			bcopy (sb, &sb[k], j * 2 * sizeof(*sb));

			for (x = 0; x < k; ) {
				if (! (sb[x++] = calloc (cols, sizeof(**sb))))
					SysError(ERROR_RESIZROW3);
			}

			/* do the same thing with the alternate buffer */
			if (ab) {
				ab = (ScrnBuf) realloc (ab, rows*2*sizeof(*ab));
				if (! ab)
					SysError (ERROR_RESIZE);

				bcopy (ab, &ab[k], (screen->max_row + 1) *
						   2 * sizeof(*ab));
				for (x = 0; x < k; ) {
					if (! (ab[x++] = calloc (cols,
							  sizeof(**ab))))
						SysError(ERROR_RESIZROW4);
				}
			}
		}
		screen->allbuf = sb;
		screen->buf = &sb[2 * savelines];
		screen->altbuf = ab;
	
		/* add the new lines to the old screen size & pos */
		screen->max_row -= index;
		if (! screen->instatus)
			screen->cur_row -= index;
		else
			screen->cur_row = screen->max_row + 1;

		screen->max_row = rows - 1;
		screen->max_col = cols - 1;
	
		/* adjust scrolling region */
		screen->top_marg = 0;
		screen->bot_marg = screen->max_row;
		*flags &= ~ORIGIN;
	
		if (screen->cur_row > screen->max_row)
			screen->cur_row = screen->max_row;
		else if (screen->cur_row < 0)
			screen->cur_row = 0;
		if (screen->cur_col > screen->max_col)
			screen->cur_col = screen->max_col;
		else if (screen->cur_col < 0)
			screen->cur_col = 0;
	
		screen->fullVwin.height = height - extra;
		screen->fullVwin.width = width;

		if (screen->active_icon)
		    SetIconSize( screen );

	}
	else if(FullHeight(screen) == height && FullWidth(screen) == width)
	 	return(0);	/* nothing has changed at all */

	if(screen->sb)
		ResizeScrollBar(screen->sb, width - SCROLLBARWIDTH,
		 Titlebar(screen) - 1, height - Titlebar(screen), rows);
	if(screen->title.tbar && FullWidth(screen) != width)
		VTTitleResize(width);
	
	screen->fullVwin.fullheight = height;
	screen->fullVwin.fullwidth = width;
#ifdef TIOCSWINSZ
	/* Set tty's idea of window size */
	ws.ws_row = rows;
	ws.ws_col = cols;
	ws.ws_xpixel = width;
	ws.ws_ypixel = height;
	ioctl (screen->respond, TIOCSWINSZ, &ws);
#ifdef SIGWINCH
	if(screen->pid > 1)
		killpg(getpgrp(screen->pid), SIGWINCH);
#endif SIGWINCH
#endif TIOCSWINSZ
	return (0);
}


SetIconSize( screen )
  Screen *screen;
{
	if (screen->active_icon) {
	    screen->iconVwin.width = (screen->max_col + 1)
				     * screen->iconVwin.f_width
	    			     + screen->border * 2;
	    screen->iconVwin.height = (screen->max_row + 1)
				      * screen->iconVwin.f_height
	    			      + screen->border * 2;
	    XChangeWindow( screen->iconVwin.window,
			   screen->iconVwin.width,
			   screen->iconVwin.height );
	} else {
	    IconRecalc( screen );
	}

	screen->iconVwin.fullwidth = screen->iconVwin.width;
	screen->iconVwin.fullheight = screen->iconVwin.height;
}
