/* Copyright (c) 1979 Regents of the University of California */
/*
 * Ex version 2
 * Bill Joy UCB
 * February, 1979
 *
 * Open and visual mode definitions.
 * 
 * There are actually 4 major states in open/visual modes.  These
 * are visual, crt open (where the cursor can move about the screen and
 * the screen can scroll and be erased), one line open (on dumb glass-crt's
 * like the adm3), and hardcopy open (for everything else).
 *
 * The basic state is given by bastate, and the current state by state,
 * since we can be in pseudo-hardcopy mode if we are on an adm3 and the
 * line is longer than 80.
 */

short	bastate;
short	state;

#define	VISUAL		0
#define	CRTOPEN		1
#define	ONEOPEN		2
#define	HARDOPEN	3

/*
 * The screen in visual and crtopen is of varying size; the basic
 * window has top basWTOP and basWLINES lines are thereby implied.
 * The current window (which may have grown from the basic size)
 * has top WTOP and WLINES lines.  The top line of the window is WTOP,
 * and the bottom line WBOT.  The line WECHO is used for messages,
 * search strings and the like.  If WBOT==WECHO then we are in ONEOPEN
 * or HARDOPEN and there is no way back to the line we were on if we
 * go to WECHO (i.e. we will have to scroll before we go there, and
 * we can't get back).  There are WCOLS columns per line.
 * If WBOT!=WECHO then WECHO will be the last line on the screen
 * and WBOT is the line before it.
 */
short	basWTOP;
short	basWLINES;
short	WTOP;
short	WBOT;
short	WLINES;
short	WCOLS;
short	WECHO;

/*
 * When we are dealing with the echo area we consider the window
 * to be "split" and set the variable splitw.  Otherwise, moving
 * off the bottom of the screen into WECHO causes a screen rollup.
 */
bool	splitw;

/*
 * Information about each line currently on the screen includes
 * the y coordinate associated with the line, the printing depth
 * of the line (0 indicates unknown), and a mask which indicates
 * whether the line is "unclean", i.e. whether we should check
 * to make sure the line is displayed correctly at the next
 * appropriate juncture.
 */
struct vlinfo {
	char	vliny;		/* Y coordinate */
	char	vdepth;		/* Depth of displayed line */
	short	vflags;		/* Is line potentially dirty ? */
} vlinfo[TUBELINES + 2];

#define	DEPTH(c)	(vlinfo[c].vdepth)
#define	LINE(c)		(vlinfo[c].vliny)
#define	FLAGS(c)	(vlinfo[c].vflags)

#define	VDIRT	1

/*
 * Hacks to copy vlinfo structures around
 */
#ifdef	V6
	/* Kludge to make up for no structure assignment */
	struct {
		long	longi;
	};
#	define	vlcopy(i, j)	i.longi = j.longi
#else
#	define	vlcopy(i, j)	i = j;
#endif

/*
 * The current line on the screen is represented by vcline.
 * There are vcnt lines on the screen, the last being "vcnt - 1".
 * Vcline is intimately tied to the current value of dot,
 * and when command mode is used as a subroutine fancy footwork occurs.
 */
short	vcline;
short	vcnt;

/*
 * To allow many optimizations on output, an exact image of the terminal
 * screen is maintained in the space addressed by vtube0.  The vtube
 * array indexes this space as lines, and is shuffled on scrolls, insert+delete
 * lines and the like rather than (more expensively) shuffling the screen
 * data itself.  It is also rearranged during insert mode across line
 * boundaries to make incore work easier.
 */
char	*vtube[TUBELINES];
char	*vtube0;

/*
 * The current cursor position within the current line is kept in
 * cursor.  The current line is kept in linebuf.  During insertions
 * we use the auxiliary array genbuf as scratch area.
 * The cursor wcursor and wdot are used in operations within/spanning
 * lines to mark the other end of the affected area, or the target
 * for a motion.
 */
char	*cursor;
char	*wcursor;
line	*wdot;

/*
 * Undo information is saved in a LBSIZE buffer at "vutmp" for changes
 * within the current line, or as for command mode for multi-line changes
 * or changes on lines no longer the current line.
 * The change kind "VCAPU" is used immediately after a U undo to prevent
 * two successive U undo's from destroying the previous state.
 */
#define	VNONE	0
#define	VCHNG	1
#define	VMANY	2
#define	VCAPU	3
#define	VMCHNG	4
#define	VMANYINS 5

short	vundkind;	/* Which kind of undo - from above */
char	*vutmp;		/* Prev line image when "VCHNG" */

/*
 * For U undo's the line is grabbed by "vmove" after it first appears
 * on that line.  The "vUNDdot" which specifies which line has been
 * saved is selectively cleared when changes involving other lines
 * are made, i.e. after a 'J' join.  This is because a 'JU' would
 * lose completely the text of the line just joined on.
 */
char	*vUNDcurs;	/* Cursor just before 'U' */
line	*vUNDdot;	/* The line address of line saved in vUNDsav */
line	vUNDsav;	/* Grabbed initial "*dot" */

#define	killU()		vUNDdot = NOLINE

/*
 * There are a number of cases where special behaviour is needed
 * from deeply nested routines.  This is accomplished by setting
 * the bits of hold, which acts to change the state of the general
 * visual editing behaviour in specific ways.
 *
 * HOLDAT prevents the clreol (clear to end of line) routines from
 * putting out @'s or ~'s on empty lines.
 *
 * HOLDDOL prevents the reopen routine from putting a '$' at the
 * end of a reopened line in list mode (for hardcopy mode, e.g.).
 *
 * HOLDROL prevents spurious blank lines when scrolling in hardcopy
 * open mode.
 *
 * HOLDQIK prevents the fake insert mode during repeated commands.
 *
 * HOLDPUPD prevents updating of the physical screen image when
 * mucking around while in insert mode.
 *
 * HOLDECH prevents clearing of the echo area while rolling the screen
 * backwards (e.g.) in deference to the clearing of the area at the
 * end of the scroll (1 time instead of n times).  The fact that this
 * is actually needed is recorded in heldech, which says that a clear
 * of the echo area was actually held off.
 */
short	hold;
short	holdupd;		/* Hold off update when echo line is too long */

#define	HOLDAT	1
#define	HOLDDOL	2
#define	HOLDROL 4
#define	HOLDQIK	8
#define	HOLDPUPD 16
#define	HOLDECH	32
#define HOLDWIG 64

/*
 * Miscellaneous variables
 */
short	CDCNT;			/* Count of ^D's in insert on this line */
char	DEL[VBSIZE];		/* Last deleted text */
bool	HADUP;			/* This insert line started with ^ then ^D */
bool	HADZERO;		/* This insert line started with 0 then ^D */
char	INS[VBSIZE];		/* Last inserted text */
short	Vlines;			/* Number of file lines "before" vi command */
short	Xcnt;			/* External variable holding last cmd's count */
bool	Xhadcnt;		/* Last command had explicit count? */
short	ZERO;
short	dir;			/* Direction for search (+1 or -1) */
short	doomed;			/* Disply chars right of cursor to be killed */
bool	gobblebl;		/* Wrapmargin space generated nl, eat a space */
bool	hadcnt;			/* (Almost) internal to vmain() */
bool	heldech;		/* We owe a clear of echo area */
bool	insmode;		/* Are in character insert mode */
char	lastcmd[5];		/* Chars in last command */
short	lastcnt;		/* Count for last command */
char	*lastcp;		/* Save current command here to repeat */
bool	lasthad;		/* Last command had a count? */
short	lastreg;		/* Register with last command */
char	*ncols['z'-'a'+1];	/* Cursor positions of marks */
char	*notenam;		/* Name to be noted with change count */
char	*notesgn;		/* Change count from last command */
char	op;			/* Operation of current command */
short	Peekkey;		/* Peek ahead key */
bool	rubble;			/* Line is filthy (in hardcopy open), redraw! */
short	vSCROLL;		/* Number lines to scroll on ^D/^U */
char	*vglobp;		/* Untyped input (e.g. repeat insert text) */
char	*vmcurs;		/* Cursor for restore after undo d), e.g. */
short	vmovcol;		/* Column to try to keep on arrow keys */
bool	vmoving;		/* Are trying to keep vmovcol */
char	vreg;			/* Register for this command */
short	wdkind;			/* Liberal/conservative words? */
char	workcmd[5];		/* Temporary for lastcmd */


/*
 * Macros
 */
#define	INF		30000
#define	LASTLINE	LINE(vcnt)
#define	OVERBUF		QUOTE
#define	beep		obeep
#define	cindent()	((outline - vlinfo[vcline].vliny) * WCOLS + outcol)
#define	vputp(cp, cnt)	tputs(cp, cnt, vputch)
#define	vputc(c)	putch(c)

/*
 * Function types
 */
int	beep();
int	qcount();
int	vchange();
int	vdelete();
int	vgrabit();
int	vinschar();
int	vmove();
int	vputchar();
int	vshift();
int	vyankit();
