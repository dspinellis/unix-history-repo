/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define MAXCOLS		256	/* maximum number of columns */

#ifndef MAC
#ifndef _TERM

/* termcap definitions */

extern char
	*UP,	/* Scroll reverse, or up */
	*CS,	/* If on vt100 */
	*SO,	/* Start standout */
	*SE,	/* End standout */
	*CM,	/* The cursor motion string */
	*CL,	/* Clear screen */
	*CE,	/* Clear to end of line */
	*HO,	/* Home cursor */
	*AL,	/* Addline (insert line) */
	*DL,	/* Delete line */
	*VS,	/* Visual start */
	*VE,	/* Visual end */
	*KS,	/* Keypad mode start */
	*KE,	/* Keypad mode end */
	*TI,	/* Cursor addressing start */
	*TE,	/* Cursor addressing end */
	*IC,	/* Insert char */
	*DC,	/* Delete char */
	*IM,	/* Insert mode */
	*EI,	/* End insert mode */
	*LL,	/* Last line, first column */
	*M_IC,	/* Insert char with arg */
	*M_DC,	/* Delete char with arg */
	*M_AL,	/* Insert line with arg */
	*M_DL,	/* Delete line with arg */
	*SF,	/* Scroll forward */
	*SR,	/* Scroll reverse */
	*SP,	/* Send cursor position */
	*VB,	/* visible bell */
	*BL,	/* audible bell */
	*IP,	/* insert pad after character inserted */
	*lPC,
	*NL;	/* newline character (usually \n) */

extern int
	LI,		/* number of lines */
	ILI,		/* number of internal lines */
	CO,		/* number of columns */

	UL,		/* underscores don't replace chars already on screen */
	MI,		/* okay to move while in insert mode */
	SG,		/* number of magic cookies left by SO and SE */

	TABS,		/* whether we are in tabs mode */
	UPlen,		/* length of the UP string */
	HOlen,		/* length of Home string */
	LLlen;		/* length of lower string */

extern char
	PC,
	*BC;		/* back space */

extern short	ospeed;

#endif /* _TERM */

#else /* MAC */	
extern int	/* probably should clean this up */
	LI,		/* number of lines */
	ILI,		/* number of internal lines */
	CO,		/* number of columns */
	TABS,
	SG;
#endif /* MAC */
