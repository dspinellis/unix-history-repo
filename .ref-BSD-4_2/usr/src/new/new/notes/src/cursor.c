static char *sccsid = "%W%";

/*
 * Terminal driving routines
 */

#include "parms.h"
#include "structs.h"

static char bufcap[256];
static char *CM;			/* cursor motion control */
static char *CL;			/* clear screen control */
static char *CE;			/* clear to end of line */
static char *SO;			/* standout mode */
static char *SE;			/* end standout mode */
static int cur_row;
static int cur_col;
static int top_col;			/* last printable character for top */
static int bot_col;			/* same for the bottom line */

char *tgoto();				/* decodes the cursor via termcap */

/*
 * charout - output 1 character
 * used by termcap tputs routine
 */
charout(c)
{
	putchar(c);
}

/*
 *  at(row,col) places cursor at row,col
 *	row = 1 to 24    ( y coords )
 * 	column = 1 to 80   ( x coords )
 */
at(row, col)
{
	register char  *p;

	if (CM) {
		if (row <= 0)
			row += nrows;		/* wraparound - cute */
		if (col <= 0)
			col += ncols;
		p = tgoto(CM, col - 1, row - 1);
		tputs(p, 1, charout);
	} else {
		if (cur_row != row)
			putchar('\n');
		else
			putchar(' ');
	}
	cur_row = row;
	cur_col = col;
}

/*
 * erase() erases the screen
 */
erase()
{
	if (CL)
		tputs(CL, 1, charout);
	else
		printf("\n\n");
	cur_row = 1;			/* back in top of screen */
	cur_col = 1;			/* back in top of screen */
	top_col = 0;
	bot_col = 0;
}

clear_eol()
{
	int i;
	int last_col;

	/*
	 * No need to do anything if there are no characters to
	 * the right (cur_col > last_col).
	 *
	 * last_col is determined in a rather crude way.
	 * We actually only keep track of the rightmost position for
	 * the bottom line (bot_col) and the lines above it (top_col)
	 */
	last_col = (cur_row == nrows) ? bot_col : top_col;
	if (cur_col > last_col)
		return;
	if (CE)
		tputs(CE, 1, charout);
	else {
		for (i = cur_col; i <= last_col; i++)
			putchar(' ');
		for (i = cur_col; i <= last_col; i++)
			putchar('\b');
	}
	/* update the line position indicators */
	if (cur_row == nrows)
		bot_col = cur_col;
	/* can't update top_col because there are many possible lines */
}

cursget()
{
	char *tgetstr();
	char *q;
	register int i;				/* rows, cols */
	char bp[1024];				/* termcap stuff */
	extern char *myterm;

	if (tgetent(bp, myterm) != 1)
		return(-1);
	q = bufcap;
	CM = tgetstr("cm", &q);			/* get the cursor motion */
	CL = tgetstr("cl", &q);			/* get the clear screen */
	CE = tgetstr("ce", &q);			/* get the clear to eol */
	SO = tgetstr("so", &q);
	SE = tgetstr("se", &q);

	if ((i = tgetnum("li")) != -1)
		nrows = i;				/* rows on screen */
	if (nrows > 24)
		nindex += nrows - 24;
	if ((i = tgetnum("co")) != -1)
		ncols = i;				/* cols on screen */
	return(0);
}

/*
 * get the coordinates of the cursor
 */
curpos(row, col)
int *row, *col;
{
	*row = cur_row;
	*col = cur_col;
}

/*
 * a putchar with no interpretation of characters
 * updates current cursor position
 */
putch(c)
{
	switch (c) {
	case '\t':
		cur_col = ((cur_col + 7) & ~07) + 1;
		break;
	case '\n':
		cur_row++;
		/* FALL INTO ... */
	case '\r':
		cur_col = 1;
		break;
	case '\b':
		if (cur_col > 1)
			cur_col--;
		break;
	default:
		if (c >= 040 && c < 0177)
			cur_col++;
		break;
	}
	putchar(c);
	/* update the line position indicators */
	if (cur_row == nrows) {
		if (cur_col > bot_col)
			bot_col = cur_col;
	} else if (cur_col > top_col)
		top_col = cur_col;
}

standout(flag)
{
	if (flag) {
		if (SO)
			tputs(SO, 1, charout);
		else
			putchar('\007');
	} else {
		if (SE)
			tputs(SE, 1, charout);
	}
}
