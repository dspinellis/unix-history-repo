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

#ifndef _CursesWindow_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _CursesWindow_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif

#include   <curses.h> 

/*
 *
 * C++ class for windows.
 *
 *
 */

class CursesWindow 
{
protected:
  static int     count;           // count of all active windows:
                                  //   We rely on the c++ promise that
                                  //   all otherwise uninitialized
                                  //   static class vars are set to 0

  WINDOW *       w;               // the curses WINDOW

  int            alloced;         // true if we own the WINDOW

  CursesWindow*  par;             // parent, if subwindow
  CursesWindow*  subwins;         // head of subwindows list
  CursesWindow*  sib;             // next subwindow of parent

  void           kill_subwindows(); // disable all subwindows

public:
                 CursesWindow(WINDOW* &window);   // useful only for stdscr

                 CursesWindow(int lines,          // number of lines
                              int cols,           // number of columns
                              int begin_y,        // line origin
                              int begin_x);       // col origin

                 CursesWindow(CursesWindow& par,  // parent window
                              int lines,          // number of lines
                              int cols,           // number of columns
                              int by,             // absolute or relative
                              int bx,             //   origins:
                              char absrel = 'a'); // if `a', by & bx are
                                                  // absolute screen pos,
                                                  // else if `r', they are
                                                  // relative to par origin
                ~CursesWindow();

// terminal status
  int            lines(); // number of lines on terminal, *not* window
  int            cols();  // number of cols  on terminal, *not* window

// window status
  int            height(); // number of lines in this window
  int            width();  // number of cols in this window
  int            begx();   // smallest x coord in window
  int            begy();   // smallest y coord in window
  int            maxx();   // largest  x coord in window
  int            maxy();   // largest  x coord in window

// window positioning
  int            move(int y, int x);

// coordinate positioning
  void           getyx(int& y, int& x);
  int            mvcur(int sy, int ey, int sx, int ex);

// input
  int            getch();
  int            getstr(char * str);
  int            scanw(const char *, ...);

// input + positioning
  int            mvgetch(int y, int x);
  int            mvgetstr(int y, int x, char * str);
  int            mvscanw(int, int, const char*, ...);

// output
  int            addch(const char ch);
  int            addstr(const char * str);
  int            printw(const char * fmt, ...);
  int            inch();
  int            insch(char c);
  int            insertln();

// output + positioning
  int            mvaddch(int y, int x, char ch);
  int            mvaddstr(int y, int x, char * str);
  int            mvprintw(int y, int x, const char * fmt, ...);
  int            mvinch(int y, int x);
  int            mvinsch(int y, int x, char ch);

// borders
  int            box(char vert, char  hor);

// erasure
  int            erase();
  int            clear();
  int            clearok(cbool bf);
  int            clrtobot();
  int            clrtoeol();
  int            delch();
  int            mvdelch(int y, int x);
  int            deleteln();

// screen control
  int            scroll();
  int            scrollok(cbool bf);
  int            touchwin();
  int            refresh();
  int            leaveok(cbool bf);
  int            flushok(cbool bf);
  int            standout();
  int            standend();

// multiple window control
  int            overlay(CursesWindow &win);
  int            overwrite(CursesWindow &win);


// traversal support
  CursesWindow*  child();
  CursesWindow*  sibling();
  CursesWindow*  parent();
};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)


inline int CursesWindow::begx()
{
  return w->_begx;
}

inline int CursesWindow::begy()
{
  return w->_begy;
}

inline int CursesWindow::maxx()
{
  return w->_maxx;
}

inline int CursesWindow::maxy()
{
  return w->_maxy;
}

inline int CursesWindow::height()
{
  return maxy() - begy() + 1;
}

inline int CursesWindow::width()
{
  return maxx() - begx() + 1;
}

inline int CursesWindow::box(char vert, char  hor)    
{
  return ::box(w, vert, hor); 
}

inline int CursesWindow::overlay(CursesWindow &win)         
{
  return ::overlay(w, win.w); 
}

inline int CursesWindow::overwrite(CursesWindow &win)       
{
  return ::overwrite(w, win.w); 
}

inline int CursesWindow::scroll()                     
{
  return ::scroll(w); 
}


inline int CursesWindow::touchwin()                   
{
  return ::touchwin(w); 
}

inline int CursesWindow::addch(const char ch)         
{
  return ::waddch(w, ch); 
}

inline int CursesWindow::addstr(const char * str)     
{
  return ::waddstr(w, str); 
}

inline int CursesWindow::clear()                      
{
  return ::wclear(w); 
}

inline int CursesWindow::clrtobot()                   
{
  return ::wclrtobot(w); 
}

inline int CursesWindow::clrtoeol()                   
{
  return ::wclrtoeol(w); 
}

inline int CursesWindow::delch()                      
{
  return ::wdelch(w); 
}

inline int CursesWindow::deleteln()                   
{
  return ::wdeleteln(w); 
}

inline int CursesWindow::erase()                      
{
  return ::werase(w); 
}

inline int CursesWindow::getch()                      
{
  return ::wgetch(w); 
}

inline int CursesWindow::getstr(char * str)           
{
  return ::wgetstr(w, str); 
}

inline int CursesWindow::inch()                       
{
  return winch(w); 
}

inline int CursesWindow::insch(char c)               
{
  return ::winsch(w, c); 
}

inline int CursesWindow::insertln()                   
{
  return ::winsertln(w); 
}

inline int CursesWindow::move(int y, int x)           
{
  return ::wmove(w, y, x); 
}


inline int CursesWindow::mvcur(int sy, int ey, int sx, int ex)
{
  return ::mvcur(sy, ey, sx,ex);
}

inline int CursesWindow::mvaddch(int y, int x, char ch)
{
  return (::wmove(w, y, x)==0) ? 0 : ::waddch(w, ch);
}

inline int CursesWindow::mvgetch(int y, int x)
{
  return (::wmove(w, y, x)==0) ? 0 : ::wgetch(w);
}

inline int CursesWindow::mvaddstr(int y, int x, char * str)
{
  return (::wmove(w, y, x)==0) ? 0 : ::waddstr(w, str);
}

inline int CursesWindow::mvgetstr(int y, int x, char * str)
{
  return (::wmove(w, y, x)==0) ? 0 : ::wgetstr(w, str);
}

inline int CursesWindow::mvinch(int y, int x)
{
  return (::wmove(w, y, x)==0) ? 0 : ::winch(w);
}

inline int CursesWindow::mvdelch(int y, int x)
{
  return (::wmove(w, y, x)==0) ? 0 : ::wdelch(w);
}

inline int CursesWindow::mvinsch(int y, int x, char ch)
{
  return (::wmove(w, y, x)==0) ? 0 : ::winsch(w, ch);
}

inline int CursesWindow::refresh()                   
{
  return ::wrefresh(w); 
}

inline int CursesWindow::clearok(cbool bf)             
{
  return ::clearok(w,bf); 
}

inline int CursesWindow::leaveok(cbool bf)             
{
  return ::leaveok(w,bf); 
}

inline int CursesWindow::scrollok(cbool bf)            
{
  return ::scrollok(w,bf); 
}

inline int CursesWindow::flushok(cbool bf)            
{
  return ::flushok(w, bf); 
}

inline void CursesWindow::getyx(int& y, int& x)       
{
  ::getyx(w, y, x); 
}

inline int CursesWindow::standout()                   
{
  return ::wstandout(w); 
}

inline int CursesWindow::standend()                   
{
  return ::wstandend(w); 
}

inline int CursesWindow::lines()                      
{
  return LINES; 
}

inline int CursesWindow::cols()                       
{
  return COLS; 
}

inline CursesWindow* CursesWindow::child()
{
  return subwins;
}

inline CursesWindow* CursesWindow::parent()
{
  return par;
}

inline CursesWindow* CursesWindow::sibling()
{
  return sib;
}


# endif
#endif
