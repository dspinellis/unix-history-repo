/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)curses.h	5.10 (Berkeley) %G%
 */

#ifndef _CURSES_H_
#define	_CURSES_H_

#include <stdio.h>

/*
 * The following #defines and #includes are present for backward
 * compatibility only.  They should not be used in future code.
 *
 * START BACKWARD COMPATIBILITY ONLY.
 */
#ifndef _CURSES_PRIVATE
#define	bool	char
#define	reg	register

#ifndef TRUE
#define	TRUE	(1)
#endif
#ifndef FALSE
#define	FALSE	(0)
#endif

#define	_puts(s)	tputs(s, 0, _putchar)
#define	_putchar(c)	__cputchar(c)

/* Old-style terminal modes access. */
#define	baudrate()	(cfgetospeed(origtermio))
#define	crmode()	cbreak()
#define	erasechar()	(origtermio.c_cc[VERASE])
#define	killchar()	(origtermio.c_cc[VKILL])
#define	nocrmode()	nocbreak()
#define	ospeed		(cfgetospeed(origtermio))
#endif /* _CURSES_PRIVATE */

extern int	 My_term;		/* Use Def_term regardless. */
extern char	*Def_term;		/* Default terminal type. */

/* END BACKWARD COMPATIBILITY ONLY. */

/* 7-bit ASCII characters. */
#define	unctrl(c)		__unctrl[(c) & 0x7f]
#define	unctrllen(ch)		__unctrllen[(ch) & 0x7f]

typedef struct _win_st {		/* Window structure. */
	short		_cury, _curx;	/* Current x, y coordinates. */
	short		_maxy, _maxx;	/* Maximum values for curx, cury. */
	short		_begy, _begx;	/* Window home. */

#define	_ENDLINE	0x001		/* End of screen. */
#define	_FLUSH		0x002		/* fflush(stdout) after refresh. */
#define	_FULLLINE	0x004		/* Line width = terminal width. */
#define	_FULLWIN	0x008		/* Window is a screen. */
#define	_IDLINE		0x010		/* Insert/delete sequences. */
#define	_SCROLLWIN	0x020		/* Last char will scroll window. */
/* 
 * XXX
 * _STANDOUT is the 8th bit, characters themselves are encoded.
 */
#define	_STANDOUT	0x080		/* Added characters are standout. */
	unsigned short	_flags;

	short		scroll;		/* Scrolling offset. */
	short		_ch_off;	/* x offset for firstch/lastch. */
	char		_clear;		/* If clear on next refresh. */
	char		_leave;		/* If cursor left. */
	char		_scroll;	/* If scrolling permitted. */
	char		**_y;		/* Line describing the window. */

#define	_NOCHANGE	-1		/* No change since last refresh. */
	short		*_firstch;	/* First and last changed in line. */
	short		*_lastch;
	struct _win_st	*_nextp, *_orig;/* Subwindows list and parent. */
} WINDOW;

/* Termcap capabilities. */
extern char	AM, BS, CA, DA, EO, HC, HZ, IN, MI, MS, NC, NS, OS,
		PC, UL, XB, XN, XT, XS, XX;
extern char	*AL, *BC, *BT, *CD, *CE, *CL, *CM, *CR, *CS, *DC, *DL,
		*DM, *DO, *ED, *EI, *K0, *K1, *K2, *K3, *K4, *K5, *K6,
		*K7, *K8, *K9, *HO, *IC, *IM, *IP, *KD, *KE, *KH, *KL,
		*KR, *KS, *KU, *LL, *MA, *ND, *NL, *RC, *SC, *SE, *SF,
		*SO, *SR, *TA, *TE, *TI, *UC, *UE, *UP, *US, *VB, *VS,
		*VE,
		*AL_PARM, *DL_PARM, *UP_PARM, *DOWN_PARM, *LEFT_PARM,
		*RIGHT_PARM;

/* Curses external declarations. */
extern WINDOW	*curscr;		/* Current screen. */
extern WINDOW	*stdscr;		/* Standard screen. */

extern struct termios origtermio;	/* Original terminal modes. */

extern int	 COLS;			/* Columns on the screen. */
extern int	 LINES;			/* Lines on the screen. */

extern char	 GT;			/* Gtty indicates tabs. */
extern char	 NONL;			/* Term can't hack LF doing a CR. */
extern char	 UPPERCASE;		/* Terminal is uppercase only. */
extern char	*ttytype;		/* Full name of current terminal. */
extern char	*__unctrl[0x80];	/* Control strings. */
extern char	 __unctrllen[0x80];	/* Control strings length. */

#define	ERR	(0)			/* Error return. */
#define	OK	(1)			/* Success return. */

/* Standard screen pseudo functions. */
#define	addbytes(da, co)	waddbytes(stdscr, da, co)
#define	addch(ch)		waddch(stdscr, ch)
#define	addstr(str)		waddbytes(stdscr, str, strlen(str))
#define	clear()			wclear(stdscr)
#define	clrtobot()		wclrtobot(stdscr)
#define	clrtoeol()		wclrtoeol(stdscr)
#define	delch()			wdelch(stdscr)
#define	deleteln()		wdeleteln(stdscr)
#define	erase()			werase(stdscr)
#define	getch()			wgetch(stdscr)
#define	getstr(str)		wgetstr(stdscr, str)
#define	inch()			winch(stdscr)
#define	insch(ch))		winsch(stdscr, ch)
#define	insertln()		winsertln(stdscr)
#define	move(y, x)		wmove(stdscr, y, x)
#define	refresh()		wrefresh(stdscr)
#define	standend()		wstandend(stdscr)
#define	standout()		wstandout(stdscr)

/* Standard screen plus movement pseudo functions. */
#define	mvaddbytes(y, x, da, co) \
				mvwaddbytes(stdscr, y, x, da, co)
#define	mvaddch(y, x, ch)	mvwaddch(stdscr, y, x, ch)
#define	mvaddstr(y, x, str)	mvwaddstr(stdscr, y, x, str)
#define	mvdelch(y, x)		mvwdelch(stdscr, y, x)
#define	mvgetch(y, x)		mvwgetch(stdscr, y, x)
#define	mvgetstr(y, x, str)	mvwgetstr(stdscr, y, x, str)
#define	mvinch(y, x)		mvwinch(stdscr, y, x)
#define	mvinsch(y, x, c)	mvwinsch(stdscr, y, x, c)
#define	mvwaddbytes(win, y, x, da, co) \
				(wmove(win, y, x) == ERR ? \
				    ERR : waddbytes(win, da, co))
#define	mvwaddch(win, y, x, ch)	(wmove(win, y, x) == ERR ? \
				    ERR : waddch(win, ch))
#define	mvwaddstr(win, y, x, str) \
				(wmove(win, y, x) == ERR ? \
				    ERR : waddbytes(win, str, strlen(str)))
#define	mvwdelch(win, y, x)	(wmove(win, y, x) == ERR ? ERR : wdelch(win))
#define	mvwgetch(win, y, x)	(wmove(win, y, x) == ERR ? ERR : wgetch(win))
#define	mvwgetstr(win, y, x, str) \
				(wmove(win, y, x) == ERR ? \
				    ERR : wgetstr(win, str))
#define	mvwinch(win, y, x)	(wmove(win, y, x) == ERR ? ERR : winch(win))
#define	mvwinsch(win, y, x, c)	(wmove(win, y, x) == ERR ? ERR : winsch(win, c))

/* Random psuedo functions. */
#define	clearok(win, bf) 	(win->_clear = (bf))
#define	flushok(win, bf)	((bf) ? (win->_flags |= _FLUSH) : \
				    (win->_flags &= ~_FLUSH))
#define	getyx(win, y, x)	(y) = win->_cury, (x) = win->_curx
#define	leaveok(win, bf)	(win->_leave = (bf))
#define	scrollok(win, bf)	(win->_scroll = (bf))
#define	winch(win)		(win->_y[win->_cury][win->_curx] & 0177)

/* Public function prototypes. */
void	 __cputchar __P((int));
int	 _sprintw __P((WINDOW *, const char *, _BSD_VA_LIST_));
int	 box __P((WINDOW *, int, int));
int	 cbreak __P((void));
int	 delwin __P((WINDOW *));
int	 echo __P((void));
int	 endwin __P((void));
char	*fullname __P((char *, char *));
char	*getcap __P((char *));
int	 gettmode __P((void));
void	 idlok __P((WINDOW *, int));
WINDOW	*initscr __P((void));
char	*longname __P((char *, char *));
int	 mvcur __P((int, int, int, int));
int	 mvprintw __P((int, int, const char *, ...));
int	 mvscanw __P((int, int, const char *, ...));
int	 mvwin __P((WINDOW *, int, int));
int	 mvwprintw __P((WINDOW *, int, int, const char *, ...));
int	 mvwscanw __P((WINDOW *, int, int, const char *, ...));
WINDOW	*newwin __P((int, int, int, int));
int	 nl __P((void));
int	 nocbreak __P((void));
int	 noecho __P((void));
int	 nonl __P((void));
int	 noraw __P((void));
int	 overlay __P((WINDOW *, WINDOW *));
int	 overwrite __P((WINDOW *, WINDOW *));
int	 printw __P((const char *, ...));
int	 raw __P((void));
int	 resetty __P((void));
int	 savetty __P((void));
int	 scanw __P((const char *, ...));
int	 scroll __P((WINDOW *));
int	 setterm __P((char *));
int	 sscans __P((WINDOW *, const char *, _BSD_VA_LIST_));
WINDOW	*subwin __P((WINDOW *, int, int, int, int));
int	 suspendwin __P((void));
int	 touchline __P((WINDOW *, int, int, int));
int	 touchoverlap __P((WINDOW *, WINDOW *));
int	 touchwin __P((WINDOW *));
void	 tstp __P((int));
int	 waddch __P((WINDOW *, int));
int	 waddstr __P((WINDOW *, char *));
int	 wclear __P((WINDOW *));
int	 wclrtobot __P((WINDOW *));
int	 wclrtoeol __P((WINDOW *));
int	 wdelch __P((WINDOW *));
int	 wdeleteln __P((WINDOW *));
int	 werase __P((WINDOW *));
int	 wgetch __P((WINDOW *));
int	 wgetstr __P((WINDOW *, char *));
int	 winsch __P((WINDOW *, int));
int	 winsertln __P((WINDOW *));
int	 wmove __P((WINDOW *, int, int));
int	 wprintw __P((WINDOW *, const char *, ...));
int	 wrefresh __P((WINDOW *));
int	 wscanw __P((WINDOW *, const char *, ...));
char	*wstandend __P((WINDOW *));
char	*wstandout __P((WINDOW *));

#ifdef _CURSES_PRIVATE
/* Private function prototypes. */
void	 __id_subwins __P((WINDOW *));
void	 __set_subwin __P((WINDOW *, WINDOW *));
void	 __swflags __P((WINDOW *));
void	 __TRACE __P((const char *, ...));
int	 waddbytes __P((WINDOW *, char *, int));

/* Private #defines. */
#define	min(a,b)	(a < b ? a : b)
#define	max(a,b)	(a > b ? a : b)

/* Private externs. */
extern int	 __echoit;
extern int	 __endwin;
extern int	 __pfast;
extern int	 __rawmode;
#endif

/* Termcap functions. */
int	 tgetent __P((char *, char *));
int	 tgetnum __P((char *));
int	 tgetflag __P((char *));
char	*tgetstr __P((char *, char **));
char	*tgoto __P((char *, int, int));
int	 tputs __P((char *, int, void (*)(int)));

#endif /* !_CURSES_H_ */
