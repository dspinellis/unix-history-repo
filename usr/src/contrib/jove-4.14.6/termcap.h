/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define MAXCOLS		256	/* maximum number of columns */

#ifdef	TERMCAP

/* termcap definitions */

extern char
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
	*M_SF,	/* Scroll forwards with arg */
	*M_SR,	/* Scroll backwards with arg */
	*SP,	/* Send cursor position */
	*VB,	/* visible bell */
	*BL,	/* audible bell */
	*IP,	/* insert pad after character inserted */
	*lPC,
	*NL,	/* newline character (usually \n) */
	*DO;

extern int
	LI,		/* number of lines */
	ILI,		/* number of internal lines */
	CO,		/* number of columns */

	SG,		/* number of magic cookies left by SO and SE */
	UPlen,		/* length of the UP string */
	HOlen,		/* length of Home string */
	LLlen;		/* length of lower string */

extern bool
	XS,	/* whether standout is braindamaged */
	Hazeltine,		/* Hazeltine tilde kludge */
	MI,		/* okay to move while in insert mode */
	UL,		/* underscores don't replace chars already on screen */
	NP,	/* there is No Pad character */ 
	TABS;		/* whether we are in tabs mode */

extern char
	PC,
	*BC,		/* back space */
	*UP;	/* Scroll reverse, or up */

extern short	ospeed;

#else	/* !TERMCAP */

extern int	/* probably should clean this up */
	LI,		/* number of lines */
	ILI,		/* number of internal lines */
	CO,		/* number of columns */
	TABS,
	SG;

#endif	/* !TERMCAP */
