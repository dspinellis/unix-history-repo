#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

/* screen.c */

#ifndef lint
static char *rcsid_screen_c = "$Header: screen.c,v 10.13 86/02/01 16:07:00 tony Rel $";
#endif	lint

#include <X/Xlib.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include "ptyx.h"

ScrnBuf Allocate (nrow, ncol)
/*
   allocates memory for a 2-dimensional array of shorts and returns a pointer thereto
 */
register int nrow, ncol;
{
	register ScrnBuf base;

	if ((base = (ScrnBuf) calloc (nrow, sizeof (short *))) == 0) Error ();

	for (nrow--; nrow >= 0; nrow--)
	   if ((base [nrow] = (short *) calloc (ncol, sizeof (short))) == 0) Error ();

	return (base);
}

ScreenWrite (screen, str, flags, length)
/*
   Writes str into buf at row row and column col.  Characters are set to match flags.
 */
Screen *screen;
register char *str;
unsigned flags;
register int length;		/* length of string */
{
	register short mask = 0;
	register short *row = screen->buf [screen->cur_row];
	register short *col = row + screen->cur_col;
	register int avail  = screen->max_col - screen->cur_col + 1;
	if (length > avail)
	    length = avail;
	if (length <= 0) return;

	if (flags & INVERSE) mask |= INVERSEbit;
	if (flags & BOLD) mask |= BOLDbit;

	if (mask)
		do { *col++ = *str++ | mask; } while (--length > 0);
	else
		do { *col++ = *str++; } while (--length > 0);
}

char *ScrnGetChars (sb, row, col, max, ptr, flags)
/*
   Stores characters from sb at row, col into *ptr.  At most max
   characters are stored; will stop if the characters in sb do not match
   the characteristics of flags.  Returns the number of characters actually
   stored + ptr.

   Requires max + col - 1 <= the maximum size of a row in sb.
   	    max >= 0
 */
ScrnBuf sb;
int row, col;
register int max;
register char *ptr;
unsigned int flags;
{
	register short mask = 0;
	register short *fetch = sb [row] + col;

	if (flags & INVERSE) mask |= INVERSEbit;
	if (flags & BOLD) mask |= BOLDbit;

	while (((*fetch & (short) 0xff00) == mask) && (max-- > 0))
	{
		if (*fetch == 0) *fetch = (short) ' ';
		*(ptr++) = (char) (*(fetch++) & (short) 0xff);
	}

	return (ptr);
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
	register short *save [MAX_ROWS];
	int length = size * sizeof (short);

	/* save n lines at bottom */
	bcopy ((char *) &sb [last - n + 1], (char *) save,
		n * sizeof (short *));
	
	/* clear contents of old rows */
	for (i = 0; i < n; i++)
		bzero ((char *) save [i], length);

	/* move down lines */
	bcopy ((char *) &sb [where], (char *) &sb [where + n],
		(last - where - n + 1) * sizeof (short *));

	/* reuse storage for new lines at where */
	bcopy ((char *) save, (char *) &sb [where], n * sizeof (short *));
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
	register short *save [MAX_ROWS];
	int length = size * sizeof (short);

	/* save n lines at where */
	bcopy ((char *) &sb [where], (char *) save, n * sizeof (short *));

	/* clear contents of old rows */
	for (i = 0; i < n; i++)
		bzero ((char *) save [i], length);

	/* move up lines */
	bcopy ((char *) &sb [where + n], (char *) &sb [where],
		(last - where - n + 1) * sizeof (short *));

	/* reuse storage for new bottom lines */
	bcopy ((char *) save, (char *) &sb [last - n + 1],
		n * sizeof (short *));
}


ScrnInsertChar (sb, row, col, n, size)
/*
   Inserts n blanks in sb at row, col.  Size is the size of each row.
 */
ScrnBuf sb;
int row, size;
register int col, n;
{
	register short *ptr = sb [row];
	register int i;

	for (i = size - 1; i >= col + n; i--)
		ptr [i] = ptr [i - n];

	bzero (ptr + col, n * sizeof (short));
}


ScrnDeleteChar (sb, row, col, n, size)
/*
   Deletes n characters in sb at row, col. Size is the size of each row.
 */
ScrnBuf sb;
register int row, size;
register int n, col;
{
	register short *ptr = sb [row];
	register nbytes;
	nbytes = (size - n - col) * sizeof (short);

	bcopy (ptr + col + n, ptr + col, nbytes);
	bzero (ptr + size - n, n * sizeof (short));
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
	char str [MAX_COLS];
	int y = toprow * screen->f_height + screen->border;
	register int row;
	int maxrow = toprow + nrows - 1;

	for (row = toprow; row <= maxrow; y += screen->f_height, row++)
	{
	   register short *chars = screen->buf [row];
	   register int col = leftcol;
	   int maxcol = leftcol + ncols - 1;
	   int lastind, curind;
	   unsigned short flags;
	   int gxfunction;
	   Font fnt;
	   int x;
	   short s;
#ifdef JUMPSCROLL
	   curind = row - screen->scroll_amt;
	   if (curind < 0 || curind > screen->max_row)
	   	continue;
	   chars = screen->buf [curind];
#else
	   chars = screen->buf [row];
#endif JUMPSCROLL

	   while (col <= maxcol && ((s = (chars[col] & ~BOLDbit)) == 0 ||
				     s == ' '))
		col++;

	   while (col <= maxcol && ((s = (chars[maxcol] & ~BOLDbit)) == 0 ||
				     s == ' '))
		maxcol--;

	   if (col > maxcol) continue;

	   flags = (chars [col] & ~CHAR);

	   if (flags & BOLDbit) fnt = screen->fnt_bold;
	   else fnt = screen->fnt_norm;

	   x = col * screen->f_width + screen->border;
	   lastind = curind = 0;

	   for (; col <= maxcol; col++, curind++)
	   {
		s = chars [col];

		if ((s & ~CHAR) != flags)
		{
		   if (flags & INVERSEbit)
		   	XText (screen->window, x, y, &str[lastind],
				curind-lastind, fnt,
		   		screen->background, screen->foreground);
		   else
		 	XText (screen->window, x, y, &str[lastind],
				curind-lastind, fnt,
		   		screen->foreground, screen->background);

		   x += (curind - lastind) * screen->f_width;

		   lastind = curind;

		   flags = (s & ~CHAR);

		   if (flags & BOLDbit) fnt = screen->fnt_bold;
		   else fnt = screen->fnt_norm;
		}

		if ((str[curind] = (char) (s & CHAR)) == 0) str[curind] = ' ';
	   }

	   if (flags & INVERSEbit)
	   	XText (screen->window, x, y, &str[lastind], curind - lastind,
	   		fnt, screen->background, screen->foreground);
	   else
	   	XText (screen->window, x, y, &str[lastind], curind - lastind,
	   		fnt, screen->foreground, screen->background);
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
	while (first <= last)
		bzero (screen->buf [first++], sizeof (short) *
					      (screen->max_col + 1));
}

ScreenResize (screen, width, height, flags)
/*
   Resizes screen:
   1. If new window would have fractional characters, sets window size so as to discard fractional characters and returns -1.
      Minimum screen size is 1 X 1.
      Note that this causes another ExposeWindow event.
   2. Enlarges screen->buf if necessary.  New space is appended to the bottom and to the right
   3. Reduces  screen->buf if necessary.  Old space is removed from the bottom and from the right
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
	register ScrnBuf sb = screen->buf;
	double scale_x, scale_y;
#ifdef TIOCSWINSZ
	struct winsize ws;
#endif	

	/* round so that it is unlikely the screen will change size on  */
	/* small mouse movements.					*/
	rows = (height + screen->f_height / 2 - 2 * screen->border) / 
		screen->f_height;
	cols = (width + screen->f_width / 2 - 2 * screen->border) / 
		screen->f_width;
	if (rows < 1) rows = 1;
	if (cols < 1) cols = 1;

	if ((width - screen->border * 2) % screen->f_width != 0 ||
		(height - screen->border * 2) % screen->f_height != 0) {
		int nwidth = cols * screen->f_width + screen->border * 2;
		int nheight = rows * screen->f_height + screen->border * 2;

		XChangeWindow (screen->window, nwidth, nheight);
		return (-1);
	}

	/* don't change anything if the screen has not changed size */
	if (screen->max_row == rows - 1 && screen->max_col == cols - 1)
		return (0);

	/* resize current lines */
	if (sb)	for (index = 0; index <= screen->max_row; index++) {
		if ((sb [index] = (short *) realloc ((char *) sb [index],
			cols * sizeof (short)))	== NULL) Error ();
		if (cols > (screen->max_col + 1))
			bzero (sb [index] + screen->max_col + 1, 
			    sizeof(short) *(cols - (screen->max_col + 1)));
	}

	/* discard excess bottom rows */
	for (index = rows; index <= screen->max_row; index++)
	   free (sb [index]);

	/* resize sb */
	if (sb == NULL)
	    sb = (ScrnBuf) malloc (rows * sizeof (short *));
	else
	    sb = (ScrnBuf) realloc (sb, rows * sizeof (short *));
	if (sb == NULL)
	    Error ();
	screen->buf = sb;

	/* create additional bottom rows as required */
	for (index = screen->max_row + 1; index < rows; index++)
	   if ((sb [index] = (short *) calloc (cols, sizeof (short))) == NULL)
	   	Error ();

	screen->max_row = rows - 1;
	screen->max_col = cols - 1;

	/* adjust scrolling region */
	screen->top_marg = 0;
	screen->bot_marg = screen->max_row;
	*flags &= ~ORIGIN;
	
	if (screen->cur_row > screen->max_row)
		screen->cur_row = screen->max_row;
	if (screen->cur_col > screen->max_col)
		screen->cur_col = screen->max_col;

	screen->height = height - 2 * screen->border;
	screen->width = width - 2 * screen->border;

	/* Set Tektronix scale factor */
	scale_x = screen->width / 4096.0;
	scale_y = screen->height / 3128.0;
	screen->TekScale = (scale_x < scale_y) ? scale_x : scale_y;

#ifdef TIOCSWINSZ
	/* Set tty's idea of window size */
	ws.ws_row = rows;
	ws.ws_col = cols;
	ws.ws_xpixel = width;
	ws.ws_ypixel = height;
	ioctl (screen->respond, TIOCSWINSZ, &ws);
#endif	
	return (0);
}
