// This may look like C code, but it is really -*- C++ -*-

/* 
Copyright (C) 1989 Free Software Foundation
    written by Eric Newton (newton@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/

/*
 * Edited for compatablity with C++, 2/28/89 <ecn>
 * This file has all the /usr/include/curses.h info (with proper prototypes)
 * used for the CursesWindow classes.  You may need to change this to be
 * compatable with your curses implementation.
 *
 */

#ifndef _curses_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _curses_h

# include   <stddef.h>
# include   <std.h>
# include   <stdio.h> 
extern "C" {
#ifndef VMS
# include   <sgtty.h>
#endif

// protection against possibility that these are macros:

#ifndef stty
extern int stty(int, struct sgttyb*); 
#endif

#ifndef gtty
extern int gtty(int, struct sgttyb*); 
#endif

}

typedef char cbool; // curses explicitly declares bools as chars

#if defined(hpux)
enum CursesStatus { ERR = -1, OK = 0 };		// curses lib uses define's
#else
enum CursesStatus { ERR = 0, OK = 1 };		// curses lib uses define's
#endif

/*
 *  BSD'ish.  Warning!!
 *
 */
# define    _ENDLINE    001
# define    _FULLWIN    002
# define    _SCROLLWIN  004
# define    _FLUSH      010
# define    _FULLLINE   020
# define    _IDLINE     040
# define    _STANDOUT   0200
# define    _NOCHANGE   -1

# define    _puts(s)    tputs(s, 0, _putchar)

/*
 * Capabilities from termcap
 */

extern cbool     AM, BS, CA, DA, DB, EO, HC, HZ, IN, MI, MS, NC, NS, OS, UL,
        XB, XN, XT, XS, XX;
extern char *AL, *BC, *BT, *CD, *CE, *CL, *CM, *CR, *CS, *DC, *DL,
        *DM, *DO, *ED, *EI, *K0, *K1, *K2, *K3, *K4, *K5, *K6,
        *K7, *K8, *K9, *HO, *IC, *IM, *IP, *KD, *KE, *KH, *KL,
        *KR, *KS, *KU, *LL, *MA, *ND, *NL, *RC, *SC, *SE, *SF,
        *SO, *SR, *TA, *TE, *TI, *UC, *UE, *UP, *US, *VB, *VS,
        *VE, *AL_PARM, *DL_PARM, *UP_PARM, *DOWN_PARM,
        *LEFT_PARM, *RIGHT_PARM;

extern char PC;

extern cbool    GT, NONL, UPPERCASE, normtty, _pfast;

struct _win_st {
#ifdef VMS
    int		_cury, _curx;
    int		_maxy, _maxx;
    int		_begy, _begx;
#else
    short       _cury, _curx;
    short       _maxy, _maxx;
    short       _begy, _begx;
#endif
    short       _flags;
#ifndef VMS
    short       _ch_off;
#endif
    cbool       _clear;
    cbool       _leave;
    cbool       _scroll;
#ifdef VMS
    cbool	_wrap;
#endif
    char        **_y;
    short       *_firstch;
    short       *_lastch;
    struct _win_st  *_nextp, *_orig;
#ifdef VMS
    struct _win_st  *_parent , *_child;
    int		_id;
#endif
};

#define WINDOW  struct _win_st

extern cbool My_term;
extern cbool _echoit;
extern cbool _rawmode;
extern cbool _endwin;

extern char *Def_term;
extern char  ttytype[];

extern int  LINES;
extern int  COLS; 
extern int  _tty_ch;
extern int  _res_flg;


typedef struct sgttyb SGTTY;

extern SGTTY _tty;

/*
 * standard curses functions.
 *
 */

extern "C"
{
extern WINDOW * stdscr;
extern WINDOW * curscr;
WINDOW * newwin(int lines, int cols, int sy, int sx);
WINDOW * subwin(WINDOW *w, int lines, int cols, int sy, int sx);
WINDOW * initscr();
int      box (WINDOW*, char, char);
int      delwin(WINDOW*);
int      mvcur(int, int, int, int);
int      overlay(WINDOW*, WINDOW*);
int      overwrite(WINDOW*, WINDOW*);
int      scroll(WINDOW*);
int      touchwin(WINDOW*);
int      waddch(WINDOW*, char);
int      waddstr(WINDOW*, const char*);
int      wclear(WINDOW*);
int      wclrtobot(WINDOW*);
int      wclrtoeol(WINDOW*);
int      wdelch(WINDOW*);
int      wdeleteln(WINDOW*);
int      werase(WINDOW*);
int      wgetch(WINDOW*);
int      wgetstr(WINDOW*, char*);
int      winsch(WINDOW*, char);
int      winsertln(WINDOW*);
int      wmove(WINDOW*, int, int);
int      wrefresh(WINDOW*);
int      wstandend(WINDOW*);
int      wstandout(WINDOW*);
int      wprintw(WINDOW*, const char * fmt, ...);
int      mvwprintw(WINDOW*, int y, int x, const char * fmt, ...);
int      wscanw(WINDOW*, const char *, ...);
int      mvwscanw(int, int, WINDOW*, const char*, ...);
int      endwin();
}

/* Pseudo functions */
/* 
 * these are inlines rather than defines here so as to allow overloaded
 * versions in the CursesWindow class
 */

int clearok(WINDOW* win, cbool bf);
int leaveok(WINDOW* win, cbool bf);
int scrollok(WINDOW* win, cbool bf);
int flushok(WINDOW* win, cbool bf);
void getyx(WINDOW* win, int& y, int& x);
int winch(WINDOW* win);

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline int clearok(WINDOW* win, cbool bf)  { return(win->_clear = bf); }
inline int leaveok(WINDOW* win, cbool bf)  { return(win->_leave = bf); }
inline int scrollok(WINDOW* win, cbool bf) { return(win->_scroll = bf); }
inline int flushok(WINDOW* win, cbool bf)  
{ return(bf ? (win->_flags |= _FLUSH):(win->_flags &= ~_FLUSH)); }
inline void getyx(WINDOW* win, int& y, int& x)   
{ y = win->_cury; x = win->_curx; }
inline int winch(WINDOW* win)   
{return  win->_y[win->_cury][win->_curx] & 0177; }

#endif /* __OPTIMIZE__ */

#if defined(USG) || defined(VMS)
extern "C" {
int raw();
int noraw();
int cbreak();
int nocbreak();
}

#else

int raw();
int noraw();
int cbreak();
int nocbreak();

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline int raw()    
{ return _tty.sg_flags|=RAW, _pfast=_rawmode=1, stty(_tty_ch,&_tty); }
inline int noraw()  
{ return _tty.sg_flags&=~RAW,_rawmode=0,_pfast=!(_tty.sg_flags&CRMOD),stty(_tty_ch,&_tty); }
inline int cbreak() 
{ return _tty.sg_flags |= CBREAK, _rawmode = 1, stty(_tty_ch,&_tty); }
inline int nocbreak() 
{ return _tty.sg_flags &= ~CBREAK,_rawmode=0,stty(_tty_ch,&_tty); }

#endif /* __ OPTIMIZE __ */

#endif /* USG */


int crmode();
int nocrmode();

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline int crmode() { return  cbreak(); }
inline int nocrmode() { return  nocbreak(); }

#endif /* __ OPTIMIZE __ */

#if defined(USG) || defined(VMS)

extern "C" {
int _setecho(int);
int _setnonl(int);
}

int echo();
int noecho();
int nl();
int nonl();

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline int echo()   { return _setecho(1); }
inline int noecho() { return _setecho(0); }
inline int nl()     { return _setnonl(0); }
inline int nonl()   { return _setnonl(1); }

#endif /* __ OPTIMIZE __ */

extern "C" {
int savetty();
int resetty();
int erasechar();
int killchar();
int baudrate();
}

#else /* not USG */

int echo();
int noecho();
int nl();
int nonl();
int savetty();
inline int resetty();
int erasechar();
int killchar();
int baudrate();

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline int echo()   
{ return _tty.sg_flags |= ECHO, _echoit = 1, stty(_tty_ch, &_tty); }
inline int noecho() 
{ return _tty.sg_flags &= ~ECHO, _echoit = 0, stty(_tty_ch, &_tty); }
inline int nl()     
{ return _tty.sg_flags |= CRMOD,_pfast = _rawmode,stty(_tty_ch, &_tty); }
inline int nonl()   
{ return _tty.sg_flags &= ~CRMOD, _pfast = 1, stty(_tty_ch, &_tty); }
inline int savetty() 
{ return (void) gtty(_tty_ch, &_tty), _res_flg = _tty.sg_flags; }
inline int resetty() 
{ return _tty.sg_flags = _res_flg, stty(_tty_ch, &_tty); }
inline int erasechar() 
{ return _tty.sg_erase; }
inline int killchar()  
{ return _tty.sg_kill; }
inline int baudrate()  
{ return _tty.sg_ospeed; }

#endif /* __ OPTIMIZE __ */

#endif /* USG */

extern "C" {
char *longname(char *, char *);
char *getcap(char *);
extern char *_unctrl[];
}


char * unctrl(int c);

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline char * unctrl(int c) {  return _unctrl[(c) & 0177]; }

#endif /* __ OPTIMIZE __ */

#endif
