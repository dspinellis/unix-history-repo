/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)curses.h	5.19 (Berkeley) %G%
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

#define	_puts(s)	tputs(s, 0, __cputchar)
#define	_putchar(c)	__cputchar(c)

/* Old-style terminal modes access. */
#define	baudrate()	(cfgetospeed(&origtermio))
#define	crmode()	cbreak()
#define	erasechar()	(origtermio.c_cc[VERASE])
#define	killchar()	(origtermio.c_cc[VKILL])
#define	nocrmode()	nocbreak()
#define	ospeed		(cfgetospeed(&origtermio))
#endif /* _CURSES_PRIVATE */

extern int	 My_term;		/* Use Def_term regardless. */
extern char	*Def_term;		/* Default terminal type. */

/* END BACKWARD COMPATIBILITY ONLY. */

/* 7-bit ASCII characters. */
#define	unctrl(c)		__unctrl[(c) & 0x7f]
#define	unctrllen(ch)		__unctrllen[(ch) & 0x7f]

/*
 * A window an array of __LINE structures pointed to by the 'lines' pointer.
 * A line is an array of __LDATA structures pointed to by the 'line' pointer.
 *
 * IMPORTANT: the __LDATA structure must NOT induce any padding, so if new
 * fields are added -- padding fields with *constant values* should ensure 
 * that the compiler will not generate any padding when storing an array of
 *  __LDATA structures.  This is to enable consistent use of bcmp, and bcopy
 * for comparing and copying arrays.
 */

typedef struct {
	char ch;			/* the actual character */

#define	__STANDOUT	0x01  		/* Added characters are standout. */
	char attr;			/* attributes of character */
} __LDATA;

#define __LDATASIZE	(sizeof(__LDATA))

typedef struct {
#define	__ISDIRTY	0x01		/* Line is dirty. */
#define __ISPASTEOL	0x02		/* Cursor is past end of line */
#define __FORCEPAINT	0x04		/* Force a repaint of the line */
	u_int flags;
	u_int hash;			/* Hash value for the line. */
	size_t *firstchp, *lastchp;	/* First and last chngd columns ptrs */
	size_t firstch, lastch;		/* First and last changed columns. */
	__LDATA *line;			/* Pointer to the line text. */
} __LINE;

typedef struct __window {		/* Window structure. */
	struct __window	*nextp, *orig;	/* Subwindows list and parent. */
	size_t begy, begx;		/* Window home. */
	size_t cury, curx;		/* Current x, y coordinates. */
	size_t maxy, maxx;		/* Maximum values for curx, cury. */
	short ch_off;			/* x offset for firstch/lastch. */
	__LINE **lines;			/* Array of pointers to the lines */
	__LINE  *lspace;		/* line space (for cleanup) */
	__LDATA *wspace;		/* window space (for cleanup) */

#define	__ENDLINE	0x001		/* End of screen. */
#define	__FLUSH		0x002		/* Fflush(stdout) after refresh. */
#define	__FULLLINE	0x004		/* Line width = terminal width. */
#define	__FULLWIN	0x008		/* Window is a screen. */
#define	__IDLINE	0x010		/* Insert/delete sequences. */
#define	__SCROLLWIN	0x020		/* Last char will scroll window. */
#define	__SCROLLOK	0x040		/* Scrolling ok. */
#define	__CLEAROK	0x080		/* Clear on next refresh. */
#define __WSTANDOUT	0x100		/* Standout window */
#define __LEAVEOK	0x200		/* If curser left */	
	u_int flags;
} WINDOW;

/* Termcap capabilities. */
extern char	AM, BS, CA, DA, EO, HC, HZ, IN, MI, MS, NC, NS, OS,
		PC, UL, XB, XN, XT, XS, XX;
extern char	*AL, *BC, *BT, *CD, *CE, *CL, *CM, *CR, *CS, *DC, *DL,
		*DM, *DO, *ED, *EI, *K0, *K1, *K2, *K3, *K4, *K5, *K6,
		*K7, *K8, *K9, *HO, *IC, *IM, *IP, *KD, *KE, *KH, *KL,
		*KR, *KS, *KU, *LL, *MA, *ND, *NL, *RC, *SC, *SE, *SF,
		*SO, *SR, *TA, *TE, *TI, *UC, *UE, *UP, *US, *VB, *VS,
		*VE, *al, *dl, *sf, *sr,
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
#define	clearok(win, bf)  ((bf) ? (win->flags |= __CLEAROK) : \
				  (win->flags &= ~__CLEAROK))
#define	flushok(win, bf)  ((bf) ? (win->flags |= __FLUSH) : \
				  (win->flags &= ~__FLUSH))
#define	scrollok(win, bf) ((bf) ? (win->flags |= __SCROLLOK) : \
				  (win->flags &= ~__SCROLLOK))
#define	leaveok(win, bf)  ((bf) ? (win->flags |= __LEAVEOK) : \
				  (win->flags &= ~__LEAVEOK))
#define	getyx(win, y, x)	(y) = win->cury, (x) = win->curx
#define	winch(win)		(win->lines[win->cury]->line[win->curx] & 0177)

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
void	 __TRACE __P((const char *, ...));
void	 __id_subwins __P((WINDOW *));
void	 __set_subwin __P((WINDOW *, WINDOW *));
void	 __swflags __P((WINDOW *));
int	 __touchline __P((WINDOW *, int, int, int, int));
int	 __touchwin __P((WINDOW *));
char	*tscroll __P((const char *, int));
int	 waddbytes __P((WINDOW *, char *, int));

/* Private #defines. */
#define	min(a,b)	(a < b ? a : b)
#define	max(a,b)	(a > b ? a : b)

/* Private externs. */
extern int	 __echoit;
extern int	 __endwin;
extern int	 __pfast;
extern int	 __rawmode;
extern int	 __noqch;
#endif

/* Termcap functions. */
int	 tgetent __P((char *, char *));
int	 tgetnum __P((char *));
int	 tgetflag __P((char *));
char	*tgetstr __P((char *, char **));
char	*tgoto __P((char *, int, int));
int	 tputs __P((char *, int, void (*)(int)));

#endif /* !_CURSES_H_ */
