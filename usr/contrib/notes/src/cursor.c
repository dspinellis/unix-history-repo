#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: cursor.c,v 1.7 85/01/18 15:07:42 notes Rel $";
#endif	RCSIDENT

/*
 *  at(row,col) places cursor on hazeltine at row,col
 *	row = 1 to 24    ( y coords )
 * 	column = 1 to 80   ( x coords )
 *
 *	numbers <=0 will have "nrows" or "ncols" added to them so
 *	as to allow "floating" positioning relative to the bottom
 *	or right side of the screen
 *
 *	-- modified 18-nov-1981 R. Essick to handle different tty types 
 *
 */

static short    ttyinit = 0;				/* whether have gotten termcap */
static char bufspace[1024];				/* term capabilities */
static char *cm = NULL;					/* cursor motion */
static char *cls = NULL;				/* clear screen string */
static char *ti = NULL;					/* start cursor motion */
static char *te = NULL;					/* stop cursor motion */
static char *ce = NULL;					/* clear to eol */
#ifdef	USG
static char *BC,
           *UP;						/* backspace, upspave */
#else
extern char *BC;					/* backspaces */
extern char *UP;					/* up 1 line */
#endif
static int  atrow,
            atcol;					/* current position */

char   *tgoto ();					/* decodes the cursor via termcap */

/*	charout - output 1 character */
/*	used by tputs routine in at */
charout (c)
{
    putchar (c);
}


at (row, col)
{
    register char  *p;

    if (ttyinit == 0)
	cursget ();					/* initialize termcap */
    if (cm != NULL)					/* was there a cm field? */
    {
	if (row <= 0)
	    row += nrows;				/* wraparound - cute */
	if (col <= 0)
	    col += ncols;
	p = tgoto (cm, col - 1, row - 1);
	tputs (p, 1, charout);

    }
    else
	if (atrow != row)
	    printf ("\n");
	else
	    printf (" ");
    atrow = row;
    atcol = col;
}

/*
 * erase()  erases the screen
 *	modified R. Essick 18-nov-81 - allow different tty types 
 *	Modified R Kolstad Jan '84 for true erase-abort function
 */

erase ()
{
    clearerr (stdout);					/* to be sure */
    if (ttyinit == 0)
	cursget ();					/* initialize termcap */
#ifdef BSD4x
    {
#include <sgtty.h>
	int     flusher = 2;
/*
 *	2 is magic constant used in kernel to mean flush write buffers
 *	only.
 */
	ioctl (1, TIOCFLUSH, &flusher);
    }
#endif	BSD4x

    if (cls != NULL)
	tputs (cls, 1, charout);
    else
	printf ("\n\n");
    atrow = 1;						/* back in top of screen */
    atcol = 1;						/* on left hand side */
}

/*
 *	cmstart, cmstop
 *
 *	prepare fancy tty's for cursor motion stuff 
 *
 */
cmstart ()
{
    if (ttyinit == 0)
	cursget ();
    if (ti != NULL)
	tputs (ti, 1, charout);
}

cmstop ()
{
    if (ttyinit == 0)
	cursget ();
    if (te != NULL)
	tputs (te, 1, charout);
}

/*
 *	clear to end of line if the capability exists.
 *
 *	FIX FOR DUMB TERMINALS
 */

ceol ()
{
    if (ttyinit == 0)
	cursget ();
    if (ce != NULL)
	tputs (ce, 1, charout);
}

cursget ()
{
    char   *getenv (), *tgetstr ();
    char   *p,
           *q;
    extern char *histty;				/* tty type on control card */
    register int    i;					/* rows, cols */
    char    bp[1024];					/* termcap stuff */
/*	bp made dynamic to give the pdp-11 breathing room */

    ttyinit = 1;					/* say we got it */
/*
 *	ttyinit set now so don't loop on ttystop/cmstop/cursget/ttystop...
 */
    if ((p = histty) == 0)
    {
	if ((p = getenv ("TERM")) == NULL)		/* a terminal ?? */
	{
	    fprintf (stderr, "You have no TERM environmental variable.  This variable tells the\n");
	    fprintf (stderr, "system what type of terminal you are using so it's features may be used.\n");
	    fprintf (stderr, "To set this variable:\n\n");
	    fprintf (stderr, "	From csh type 'setenv TERM <term-type>'.\n");
	    fprintf (stderr, "	From sh type 'TERM=<termtype>;export TERM'.\n\n");
	    fprintf (stderr, "Where <term-type> is the system designation for your terminal.\n");
	    fprintf (stderr, "(E.g. hp2621, adm3a, aaa40, etc).\n");
	    ttystop ();					/* make sure tty is in normal state */
	    exit (BAD);					/* and terminate */
	}
    }
    if (tgetent (bp, p) != 1)
    {
	fprintf (stderr,
		"Can't find capabilities for terminal type \"%s\"\n", p);
	ttystop ();					/* reset tty states */
	exit (BAD);
    }
    q = bufspace;
    cm = tgetstr ("cm", &q);				/* get cursor motion */
    cls = tgetstr ("cl", &q);				/* clear screen */
    ti = tgetstr ("ti", &q);				/* start cursor motion */
    te = tgetstr ("te", &q);				/* stop cursor motion */
    UP = tgetstr ("up", &q);				/* cursor up */
    BC = tgetstr ("bc", &q);				/* cursor left */
    ce = tgetstr ("ce", &q);				/* clear to eol */
    if ((i = tgetnum ("li")) != -1)
	nrows = i;					/* rows on screen */
    if (nrows != 24)					/* different screen size */
    {
	Nindex = nrows - 12;				/* header, trailer, prompt */
    }
    if ((i = tgetnum ("co")) != -1)
	ncols = i;					/* cols on screen */
}

/*
 * miscellaneous output routines for the terminal
 *
 * center(p,len,row,col) takes a character string pointed at by p and centers it
 *    within a field of length n.  it is printed on screen at row,col
 *    (centered).  It is also assumed that p's string is BLANK TERMINATED
 *
 * prdate(w) struct when_f *w;  prints the date.  Assumes 'at' is already done
 *
 * sprdate(w,str) struct when_f *w; char str[]; formats the date and returns
 *		the result in the string pointed to by str.
 *
 */

center (p, len, row, col) char *p;
{
    register int    i;
    char   *r;

    i = strlen (p);					/* get length */
    if (i != 0)
    {
	at (row, col + (len - i) / 2);
	fwrite (p, sizeof (char), i, stdout);		/* write it */
    }
}
