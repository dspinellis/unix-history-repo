# include	<stdio.h>
 
# include	<sgtty.h>

# define	bool	char		/* boolean variable		*/
# define	reg	register	/* register varaible abbr.	*/

# define	TRUE	(1)
# define	FALSE	(0)
# define	ERR	(0)		/* default return on error	*/
# define	OK	(1)		/* default return on good run	*/

# define	_SUBWIN		01	/* window is a subindow		*/
# define	_ENDLINE	02	/* lines go to end of screen	*/
# define	_FULLWIN	04	/* window is entire screen	*/
# define	_SCROLLWIN	010	/* window could cause scroll	*/
# define	_STANDOUT	0200	/* standout mode in effect	*/
# define	_NOCHANGE	-1	/* no change on this line	*/

# define	_puts(s)	tputs(s, 0, _putchar);

typedef	struct sgttyb	SGTTY;

# ifndef WINDOW

/* Copyright (c) 1979 Regents of the University of California */
/*
 * Capabilities from termcap
 */

char	*AL;			/* P* Add new blank line		*/
bool	AM;			/*    Automatic margins			*/
char	*BC;			/*    Back cursor			*/
bool	BS;			/*    Backspace works			*/
char	*BT;			/* P  Back tab				*/
bool	CA;			/*    Cursor addressible		*/
char	*CD;			/* P* Clear to end of display		*/
char	*CE;			/* P  Clear to end of line		*/
char	*CL;			/* P* Clear screen			*/
char	*CM;			/* P  Cursor motion			*/
bool	DA;			/*    Display may be retained above	*/
bool	DB;			/*    Display may be retained below	*/
char	*DC;			/* P* Delete character			*/
char	*DL;			/* P* Delete line sequence		*/
char	*DM;			/*    Delete mode (enter)		*/
char	*DO;			/*    Down line sequence		*/
char	*ED;			/*    End delete mode			*/
bool	EO;			/*    Can erase overstrikes with ' '	*/
char	*EI;			/*    End insert mode			*/
bool	GT;			/*    Gtty indicates tabs		*/
char	*HO;			/*    Home cursor			*/
bool	HZ;			/*    Hazeltine ~ braindamage		*/
char	*IC;			/* P  Insert character			*/
bool	IN;			/*    Insert-null blessing		*/
char	*IM;			/*    Insrt mode (as ':im=:' if 'ic')	*/
char	*IP;			/* P* pad after char ins'd using IM+IE	*/
char	*LL;			/*    Quick to last line, column 0	*/
char	*MA;			/*    Ctrl character map for cmd mode	*/
bool	MI;			/*    can move in insert mode		*/
bool	NC;			/*    No Cr: \r sends \r\n then eats \n	*/
char	*ND;			/*    Non-destructive space		*/
bool	OS;			/*    Overstrike works			*/
char	PC;			/*    Pad character			*/
char	*SE;			/*    Standout end (may leave space)	*/
char	*SF;			/* P  Scroll forwards			*/
char	*SO;			/*    Stand out begin (may leave space)	*/
char	*SR;			/* P  Scroll backwards			*/
char	*TA;			/* P  Tab (not ^I or with padding)	*/
char	*TE;			/*    End sequence after TI		*/
char	*TI;			/*    Terminal Initialize		*/
bool	UL;			/*    Underlining works even though !os	*/
char	*UE;			/*    Underline End sequence		*/
char	*UP;			/*    Upline				*/
char	*US;			/*    Underline Start sequence		*/
char	*VB;			/*    Visible bell			*/
char	*VE;			/*    Visual end sequence		*/
char	*VS;			/*    Visual start sequence		*/
bool	XN;			/*    A newline gets eaten after wrap	*/
	/* X? is reserved for severely nauseous glitches		*/
	/* If there are enough of these we may need bit masks!		*/

/*
 * From the tty modes...
 */
bool	NONL;			/* Term can't hack linefeeds doing a CR	*/
bool	UPPERCASE;		/* Ick!					*/

bool	normtty;		/* set if must normal mode from normf	*/
bool	pfast;			/* Have stty -nl'ed to go faster	*/

# define	WINDOW	struct _win_st

struct _win_st {		/* window description structure		*/
	short	_cury, _curx;		/* current y,x positions	*/
	short	_maxy, _maxx;		/* maximum y,x positions	*/
	short	_begy, _begx;		/* start y,x positions		*/
	short	_flags;			/* various window flags		*/
	bool	_clear;			/* need to clear		*/
	bool	_leave;			/* leave curx,y at last update	*/
	bool	_scroll;		/* scrolls allowed		*/
	char	**_y;			/* actual window		*/
	short	*_firstch;		/* first change on line		*/
	short	*_lastch;		/* last change on line		*/
};

extern bool	My_term,	/* set if user species terminal		*/
		_echoit,	/* set if echoing characters		*/
		_rawmode;	/* set if terminal in raw mode		*/

extern char	*Def_term,	/* default terminal type		*/
		ttytype[];	/* long name of current terminal	*/

extern int	LINES, COLS,	/* number of lines and columns		*/
		_tty_ch,	/* channel with tty on it		*/
		_res_flg;	/* sgtty flags stored for reset		*/

# ifdef DEBUG
FILE		*outf;		/* error outfile			*/
# endif

SGTTY		_tty;		/* tty structure			*/

WINDOW		*stdscr,	/* standard screen			*/
		*curscr;	/* current screen			*/

/*
 *	Define VOID to stop lint from generating "null effect"
 * comments.
 */
# ifdef lint
int	__void__;		/* place to assign to			*/
# define	VOID(x)	(__void__ = (int) (x))
# else
# define	VOID(x)	(x)
# endif

# endif

/*
 * psuedo functions for standard screen
 */
# define	addch(ch)	VOID(waddch(stdscr, ch))
# define	getch()		VOID(wgetch(stdscr))
# define	addstr(str)	VOID(waddstr(stdscr, str))
# define	getstr(str)	VOID(wgetstr(stdscr, str))
# define	move(y, x)	VOID(wmove(stdscr, y, x))
# define	clear()		VOID(wclear(stdscr))
# define	erase()		VOID(werase(stdscr))
# define	clrtobot()	VOID(wclrtobot(stdscr))
# define	clrtoeol()	VOID(wclrtoeol(stdscr))
# define	insertln()	VOID(winsertln(stdscr))
# define	deleteln()	VOID(wdeleteln(stdscr))
# define	refresh()	VOID(wrefresh(stdscr))
# define	inch()		VOID(winch(stdscr))
# ifdef STANDOUT
# define	standout()	VOID(wstandout(stdscr))
# define	standend()	VOID(wstandend(stdscr))
# endif

/*
 * mv functions
 */
#define	mvwaddch(win,y,x,ch)	VOID(wmove(win,y,x)==ERR?ERR:waddch(win,ch))
#define	mvwgetch(win,y,x,ch)	VOID(wmove(win,y,x)==ERR?ERR:wgetch(win,ch))
#define	mvwaddstr(win,y,x,str)	VOID(wmove(win,y,x)==ERR?ERR:waddstr(win,str))
#define	mvwgetstr(win,y,x,str)	VOID(wmove(win,y,x)==ERR?ERR:wgetstr(win,str))
#define	mvwinch(win,y,x)	VOID(wmove(win,y,x) == ERR ? ERR : winch(win))
#define	mvaddch(y,x,ch)		mvwaddch(stdscr,y,x,ch)
#define	mvgetch(y,x,ch)		mvwgetch(stdscr,y,x,ch)
#define	mvaddstr(y,x,str)	mvwaddstr(stdscr,y,x,str)
#define	mvgetstr(y,x,str)	mvwgetstr(stdscr,y,x,str)
#define	mvinch(y,x)		mvwinch(win,y,x)

/*
 * psuedo functions
 */

#define	clearok(win,bf)	 (win->_clear = bf)
#define	leaveok(win,bf)	 (win->_leave = bf)
#define	scrollok(win,bf) (win->_scroll = bf)
#define	getyx(win,y,x)	 y = win->_cury, x = win->_curx
#define	winch(win)	 (win->_y[win->_cury][win->_curx])

#define raw()	 (_tty.sg_flags |= RAW, _rawmode = TRUE, stty(_tty_ch, &_tty))
#define noraw()	 (_tty.sg_flags &= ~RAW, _rawmode = FALSE, stty(_tty_ch, &_tty))
#define crmode() (_tty.sg_flags |= CBREAK, _rawmode = TRUE, stty(_tty_ch,&_tty))
#define nocrmode() (_tty.sg_flags &= ~CBREAK,_rawmode=FALSE,stty(_tty_ch,&_tty))
#define echo()	 (_tty.sg_flags |= ECHO, _echoit = TRUE, stty(_tty_ch, &_tty))
#define noecho() (_tty.sg_flags &= ~ECHO, _echoit = FALSE, stty(_tty_ch, &_tty))
#define nl()	 (_tty.sg_flags |= CRMOD, NONL = TRUE, stty(_tty_ch, &_tty))
#define nonl()	 (_tty.sg_flags &= ~CRMOD, NONL = FALSE, stty(_tty_ch, &_tty))
#define	savetty()	(gtty(_tty_ch, &_tty), _res_flg = _tty.sg_flags)
#define	resetty()	(_tty.sg_flags = _res_flg, stty(_tty_ch, &_tty))

WINDOW	*initscr(), *newwin();
