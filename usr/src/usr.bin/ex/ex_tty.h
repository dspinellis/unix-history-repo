/* Copyright (c) 1981 Regents of the University of California */
/* sccs id:	@(#)ex_tty.h	7.1	%G%  */
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
extern char	tspace[256];	/* Space for capability strings */
extern char	*aoftspace;	/* Address of tspace for relocation */

extern char	*AL;		/* P* Add new blank line */
extern char	*BC;		/*    Back cursor */
extern char	*BT;		/* P  Back tab */
extern char	*CD;		/* P* Clear to end of display */
extern char	*CE;		/* P  Clear to end of line */
extern char	*CL;		/* P* Clear screen */
extern char	*CM;		/* P  Cursor motion */
extern char	*xCR;		/* P  Carriage return */
extern char	*DC;		/* P* Delete character */
extern char	*DL;		/* P* Delete line sequence */
extern char	*DM;		/*    Delete mode (enter)  */
extern char	*DO;		/*    Down line sequence */
extern char	*ED;		/*    End delete mode */
extern char	*EI;		/*    End insert mode */
extern char	*F0,*F1,*F2,*F3,*F4,*F5,*F6,*F7,*F8,*F9;
				/*    Strings sent by various function keys */
extern char	*HO;		/*    Home cursor */
extern char	*IC;		/* P  Insert character */
extern char	*IM;		/*    Insert mode (give as ':im=:' if 'ic' */
extern char	*IP;		/* P* Insert pad after char ins'd using IM+IE */
extern char	*KD;		/*    Keypad down arrow */
extern char	*KE;		/*    Keypad don't xmit */
extern char	*KH;		/*    Keypad home key */
extern char	*KL;		/*    Keypad left arrow */
extern char	*KR;		/*    Keypad right arrow */
extern char	*KS;		/*    Keypad start xmitting */
extern char	*KU;		/*    Keypad up arrow */
extern char	*LL;		/*    Quick to last line, column 0 */
extern char	*ND;		/*    Non-destructive space */
extern char	*xNL;		/*    Line feed (new line) */
extern char	PC;		/*    Pad character */
extern char	*SE;		/*    Standout end (may leave space) */
extern char	*SF;		/* P  Scroll forwards */
extern char	*SO;		/*    Stand out begin (may leave space) */
extern char	*SR;		/* P  Scroll backwards */
extern char	*TA;		/* P  Tab (other than ^I or with padding) */
extern char	*TE;		/*    Terminal end sequence */
extern char	*TI;		/*    Terminal initial sequence */
extern char	*UP;		/*    Upline */
extern char	*VB;		/*    Visible bell */
extern char	*VE;		/*    Visual end sequence */
extern char	*VS;		/*    Visual start sequence */
extern bool	AM;		/* Automatic margins */
extern bool	BS;		/* Backspace works */
extern bool	CA;		/* Cursor addressible */
extern bool	DA;		/* Display may be retained above */
extern bool	DB;		/* Display may be retained below */
extern bool	EO;		/* Can erase overstrikes with ' ' */
extern bool	GT;		/* Gtty indicates tabs */
extern bool	HC;		/* Hard copy terminal */
extern bool	HZ;		/* Hazeltine ~ braindamage */
extern bool	IN;		/* Insert-null blessing */
extern bool	MI;		/* can move in insert mode */
extern bool	NC;		/* No Cr - \r snds \r\n then eats \n (dm2500) */
extern bool	NS;		/* No scroll - linefeed at bottom won't scroll */
extern bool	OS;		/* Overstrike works */
extern bool	UL;		/* Underlining works even though !os */
extern bool	XB;		/* Beehive (no escape key, simulate with f1) */
extern bool	XN;		/* A newline gets eaten after wrap (concept) */
extern bool	XT;		/* Tabs are destructive */
bool	XV;			/* VT100 - run AL and DL through tgoto */
extern bool	XX;		/* Tektronix 4025 insert line */
	/* X? is reserved for severely nauseous glitches */
	/* If there are enough of these we may need bit masks! */

/*
 * From the tty modes...
 */
extern bool	NONL;		/* Terminal can't hack linefeeds doing a CR */
extern bool	UPPERCASE;	/* Ick! */
extern short	LINES;		/* Number of lines on screen */
extern short	COLUMNS;
extern short	OCOLUMNS;	/* Save COLUMNS for a hack in open mode */

extern short	outcol;		/* Where the cursor is */
extern short	outline;

extern short	destcol;	/* Where the cursor should be */
extern short	destline;

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
 * is the thing we pass to sTTY and family to turn "RAW" mode on or off
 * when we go into or out of visual mode.  In V7/V6 it's just the flags word
 * to stty.  In USG V3 it's the whole tty structure.
 */
#ifdef	USG3TTY			/* USG V3 */
  extern struct	termio tty;	/* Use this one structure to change modes */
  typedef	struct termio ttymode;	/* Mode to contain tty flags */

#else				/* All others */
  extern struct	sgttyb tty;	/* Always stty/gtty using this one structure */
  typedef	int ttymode;	/* Mode to contain tty flags */
# ifdef 	TIOCSETC	/* V7 */
   extern struct	tchars ottyc, nttyc;	/* For V7 character masking */
# endif
# ifdef		TIOCLGET	/* Berkeley */
   extern struct	ltchars olttyc, nlttyc;	/* More of tchars style stuff */
# endif

#endif

extern ttymode	normf;		/* Restore tty flags to this (someday) */
extern bool	normtty;	/* Have to restore normal mode from normf */

ttymode ostart(), setty(), unixex();

extern short	WBOT;
extern short	WECHO;

extern short	costCM;	/* # chars to output a typical CM, with padding etc. */
extern short	costSR;	/* likewise */
extern short	costAL;

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
extern struct maps arrows[MAXNOMACS];	/* macro defs - 1st 5 built in */
extern struct maps immacs[MAXNOMACS];	/* for while in insert mode */
extern struct maps abbrevs[MAXNOMACS];	/* for word abbreviations */
extern int	ldisc;			/* line discipline for ucb tty driver */
extern char	mapspace[MAXCHARMACS];
extern char	*msnext;	/* next free location in mapspace */
extern int	maphopcnt;	/* check for infinite mapping loops */
extern bool	anyabbrs;	/* true if abbr or unabbr has been done */
extern char	ttynbuf[20];	/* result of ttyname() */
extern int	ttymesg;	/* original mode of users tty */
