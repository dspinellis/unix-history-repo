/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ex_tty.h	7.7 (Berkeley) 3/9/87
 */

/*
 * Capabilities from termcap
 *
 * The description of terminals is a difficult business, and we only
 * attempt to summarize the capabilities here;  for a full description
 * see the paper describing termcap.
 *
 * Capabilities from termcap are of three kinds - string valued options,
 * numeric valued options, and boolean options.  The string valued options
 * are the most complicated, since they may include padding information,
 * which we describe now.
 *
 * Intelligent terminals often require padding on intelligent operations
 * at high (and sometimes even low) speed.  This is specified by
 * a number before the string in the capability, and has meaning for the
 * capabilities which have a P at the front of their comment.
 * This normally is a number of milliseconds to pad the operation.
 * In the current system which has no true programmible delays, we
 * do this by sending a sequence of pad characters (normally nulls, but
 * specifiable as "pc").  In some cases, the pad is better computed
 * as some number of milliseconds times the number of affected lines
 * (to bottom of screen usually, except when terminals have insert modes
 * which will shift several lines.)  This is specified as '12*' e.g.
 * before the capability to say 12 milliseconds per affected whatever
 * (currently always line).  Capabilities where this makes sense say P*.
 */
var	char	tspace[1024];	/* Space for capability strings */
var	char	*aoftspace;	/* Address of tspace for relocation */

var	char	*AL;		/* P* Add new blank line */
var	char	*AL_PARM;	/* P* Add n new blank lines */
extern	char	*BC;		/*    Back cursor */
var	char	*BT;		/* P  Back tab */
var	char	*CD;		/* P* Clear to end of display */
var	char	*CE;		/* P  Clear to end of line */
var	char	*CL;		/* P* Clear screen */
var	char	*CM;		/* PG Cursor motion */
var	char	*CS;		/* PG Change scrolling region (vt100) */
var	char	*xCR;		/* P  Carriage return */
var	char	*DC;		/* P* Delete character */
var	char	*DL;		/* P* Delete line sequence */
var	char	*DL_PARM;	/* P* Delete n lines */
var	char	*DM;		/*    Delete mode (enter)  */
var	char	*DO;		/*    Down line sequence */
var	char	*DOWN_PARM;	/*    Down n lines */
var	char	*ED;		/*    End delete mode */
var	char	*EI;		/*    End insert mode */
var	char	*F0,*F1,*F2,*F3,*F4,*F5,*F6,*F7,*F8,*F9;
				/*    Strings sent by various function keys */
var	char	*HO;		/*    Home cursor */
var	char	*IC;		/* P  Insert character */
var	char	*IM;		/*    Insert mode (give as ':im=:' if 'ic' */
var	char	*IP;		/* P* Insert pad after char ins'd using IM+IE */
var	char	*KD;		/*    Keypad down arrow */
var	char	*KE;		/*    Keypad don't xmit */
var	char	*KH;		/*    Keypad home key */
var	char	*KL;		/*    Keypad left arrow */
var	char	*KR;		/*    Keypad right arrow */
var	char	*KS;		/*    Keypad start xmitting */
var	char	*KU;		/*    Keypad up arrow */
var	char	*LEFT_PARM;	/*    Left n chars */
var	char	*LL;		/*    Quick to last line, column 0 */
var	char	*ND;		/*    Non-destructive space */
var	char	*RIGHT_PARM;	/*    Right n spaces */
var	char	*xNL;		/*    Line feed (new line) */
extern	char	PC;		/*    Pad character */
var	char	*RC;		/*    Restore cursor from last SC */
var	char	*SC;		/*    Save cursor */
var	char	*SE;		/*    Standout end (may leave space) */
var	char	*SF;		/* P  Scroll forwards */
var	char	*SO;		/*    Stand out begin (may leave space) */
var	char	*SR;		/* P  Scroll backwards */
var	char	*TA;		/* P  Tab (other than ^I or with padding) */
var	char	*TE;		/*    Terminal end sequence */
var	char	*TI;		/*    Terminal initial sequence */
extern	char	*UP;		/*    Upline */
var	char	*UP_PARM;	/*    Up n lines */
var	char	*VB;		/*    Visible bell */
var	char	*VE;		/*    Visual end sequence */
var	char	*VS;		/*    Visual start sequence */
var	bool	AM;		/* Automatic margins */
var	bool	BS;		/* Backspace works */
var	bool	CA;		/* Cursor addressible */
var	bool	DA;		/* Display may be retained above */
var	bool	DB;		/* Display may be retained below */
var	bool	EO;		/* Can erase overstrikes with ' ' */
var	bool	GT;		/* Gtty indicates tabs */
var	bool	HC;		/* Hard copy terminal */
var	bool	HZ;		/* Hazeltine ~ braindamage */
var	bool	IN;		/* Insert-null blessing */
var	bool	MI;		/* can move in insert mode */
var	bool	NC;		/* No Cr - \r snds \r\n then eats \n (dm2500) */
var	bool	NS;		/* No scroll - linefeed at bottom won't scroll */
var	bool	OS;		/* Overstrike works */
var	bool	UL;		/* Underlining works even though !os */
var	bool	XB;		/* Beehive (no escape key, simulate with f1) */
var	bool	XN;		/* A newline gets eaten after wrap (concept) */
var	bool	XT;		/* Tabs are destructive */
var	bool	XX;		/* Tektronix 4025 insert line */
	/* X? is reserved for severely nauseous glitches */
	/* If there are enough of these we may need bit masks! */

/*
 * From the tty modes...
 */
var	bool	NONL;		/* Terminal can't hack linefeeds doing a CR */
var	bool	UPPERCASE;	/* Ick! */
extern	short	LINES;		/* Number of lines on screen */
extern	short	COLUMNS;
var	short	OCOLUMNS;	/* Save COLUMNS for a hack in open mode */
#ifdef	TIOCGWINSZ
var	struct winsize winsz;	/* Save window size for stopping comparisons */
#endif

var	short	outcol;		/* Where the cursor is */
var	short	outline;

var	short	destcol;	/* Where the cursor should be */
var	short	destline;

/*
 * There are several kinds of tty drivers to contend with.  These include:
 * (1)	V6:		no CBREAK, no ioctl.  (Include PWB V1 here).
 * (2)	V7 research:	has CBREAK, has ioctl, and has the tchars (TIOCSETC)
 *			business to change start, stop, etc. chars.
 * (3)	USG V2:		Basically like V6 but RAW mode is like V7 RAW.
 *			(We treat it as V6.)
 * (4)	USG V3:		equivalent to V7 but totally incompatible.
 * (5)  Berkeley:	has ltchars in addition to all of V7.
 *
 * The following attempts to decide what we are on, and declare
 * some variables in the appropriate format.  The wierd looking one (ttymode)
 * is the thing we pass to ex_sTTY and family to turn "RAW" mode on or off
 * when we go into or out of visual mode.  In V7/V6 it's just the flags word
 * to stty.  In USG V3 it's the whole tty structure.
 */
#ifdef	USG3TTY			/* USG V3 */
  var	struct	termio tty;	/* Use this one structure to change modes */
  typedef	struct termio ttymode;	/* Mode to contain tty flags */

#else				/* All others */
  var	struct	sgttyb tty;	/* Always stty/gtty using this one structure */
  typedef	int ttymode;	/* Mode to contain tty flags */
# ifdef 	TIOCSETC	/* V7 */
   var	struct	tchars ottyc, nttyc;	/* For V7 character masking */
# endif
# ifdef		TIOCLGET	/* Berkeley */
   var	struct	ltchars olttyc, nlttyc;	/* More of tchars style stuff */
# endif

#endif

var	ttymode	normf;		/* Restore tty flags to this (someday) */
var	bool	normtty;	/* Have to restore normal mode from normf */

ttymode ostart(), setty(), unixex();

var	short	costCM;	/* # chars to output a typical CM, with padding etc. */
var	short	costSR;	/* likewise for scroll reverse */
var	short	costAL;	/* likewise for insert line */
var	short	costDP;	/* likewise for DOWN_PARM */
var	short	costLP;	/* likewise for LEFT_PARM */
var	short	costRP;	/* likewise for RIGHT_PARM */

#ifdef VMUNIX
# define MAXNOMACS	128	/* max number of macros of each kind */
# define MAXCHARMACS	2048	/* max # of chars total in macros */
#else
# define MAXNOMACS	32	/* max number of macros of each kind */
# define MAXCHARMACS	512	/* max # of chars total in macros */
#endif
struct maps {
	char *cap;	/* pressing button that sends this.. */
	char *mapto;	/* .. maps to this string */
	char *descr;	/* legible description of key */
};
var	struct maps arrows[MAXNOMACS];	/* macro defs - 1st 5 built in */
var	struct maps immacs[MAXNOMACS];	/* for while in insert mode */
var	struct maps abbrevs[MAXNOMACS];	/* for word abbreviations */
var	int	ldisc;			/* line discipline for ucb tty driver */
var	char	mapspace[MAXCHARMACS];
var	char	*msnext;	/* next free location in mapspace */
var	int	maphopcnt;	/* check for infinite mapping loops */
var	bool	anyabbrs;	/* true if abbr or unabbr has been done */
var	char	ttynbuf[20];	/* result of ttyname() */
var	int	ttymesg;	/* original mode of users tty */
