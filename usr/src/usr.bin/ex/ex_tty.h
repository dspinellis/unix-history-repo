/* Copyright (c) 1979 Regents of the University of California */
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
char	tspace[256];		/* Space for capability strings */
char	*aoftspace;		/* Address of tspace for relocation */

char	*AL;			/* P* Add new blank line */
char	*BC;			/*    Back cursor */
char	*BT;			/* P  Back tab */
char	*CD;			/* P* Clear to end of display */
char	*CE;			/* P  Clear to end of line */
char	*CL;			/* P* Clear screen */
char	*CM;			/* P  Cursor motion */
char	*DC;			/* P* Delete character */
char	*DL;			/* P* Delete line sequence */
char	*DM;			/*    Delete mode (enter)  */
char	*DO;			/*    Down line sequence */
char	*ED;			/*    End delete mode */
char	*EI;			/*    End insert mode */
char	*F0,*F1,*F2,*F3,*F4,*F5,*F6,*F7,*F8,*F9;
				/*    Strings sent by various function keys */
char	*HO;			/*    Home cursor */
char	*IC;			/* P  Insert character */
char	*IM;			/*    Insert mode (give as ':im=:' if 'ic' */
char	*IP;			/* P* Insert pad after char ins'd using IM+IE */
char	*KD;			/*    Keypad down arrow */
char	*KE;			/*    Keypad don't xmit */
char	*KH;			/*    Keypad home key */
char	*KL;			/*    Keypad left arrow */
char	*KR;			/*    Keypad right arrow */
char	*KS;			/*    Keypad start xmitting */
char	*KU;			/*    Keypad up arrow */
char	*LL;			/*    Quick to last line, column 0 */
char	*ND;			/*    Non-destructive space */
char	PC;			/*    Pad character */
char	*SE;			/*    Standout end (may leave space) */
char	*SF;			/* P  Scroll forwards */
char	*SO;			/*    Stand out begin (may leave space) */
char	*SR;			/* P  Scroll backwards */
char	*TA;			/* P  Tab (other than ^I or with padding) */
char	*TE;			/*    Terminal end sequence */
char	*TI;			/*    Terminal initial sequence */
char	*UP;			/*    Upline */
char	*VB;			/*    Visible bell */
char	*VE;			/*    Visual end sequence */
char	*VS;			/*    Visual start sequence */
bool	AM;			/* Automatic margins */
bool	BS;			/* Backspace works */
bool	CA;			/* Cursor addressible */
bool	DA;			/* Display may be retained above */
bool	DB;			/* Display may be retained below */
bool	EO;			/* Can erase overstrikes with ' ' */
bool	GT;			/* Gtty indicates tabs */
bool	HC;			/* Hard copy terminal */
bool	HZ;			/* Hazeltine ~ braindamage */
bool	IN;			/* Insert-null blessing */
bool	MI;			/* can move in insert mode */
bool	NC;			/* No Cr - \r snds \r\n then eats \n (dm2500) */
bool	OS;			/* Overstrike works */
bool	UL;			/* Underlining works even though !os */
bool	XN;			/* A newline gets eaten after wrap (concept) */
bool	XT;			/* Tabs are destructive */
	/* X? is reserved for severely nauseous glitches */
	/* If there are enough of these we may need bit masks! */

/*
 * From the tty modes...
 */
bool	NONL;			/* Terminal can't hack linefeeds doing a CR */
bool	UPPERCASE;		/* Ick! */
short	LINES;			/* Number of lines on screen */
short	COLUMNS;
short	OCOLUMNS;		/* Save COLUMNS for a hack in open mode */

short	outcol;			/* Where the cursor is */
short	outline;

short	destcol;		/* Where the cursor should be */
short	destline;

#ifdef 	TIOCSETC
struct	tchars ottyc, nttyc;	/* For V7 character masking */
#endif
struct	sgttyb tty;		/* Always stty/gtty using this one structure */
bool	normtty;		/* Have to restor normal mode from normf */
int	normf;			/* Restore tty flags to this (someday) */

short	WBOT;
short	WECHO;

short	costCM;

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
struct maps arrows[MAXNOMACS];	/* macro defs - 1st 5 built in */
struct maps immacs[MAXNOMACS];	/* for while in insert mode */
char	mapspace[MAXCHARMACS];
char	*msnext;	/* next free location in mapspace */
