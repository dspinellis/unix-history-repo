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
#ifdef __GNUG__
#pragma implementation
#endif
#include <stdio.h>
#include <stdarg.h>
#include <builtin.h>
#include <values.h>
#include <CursesW.h>

extern "C" int _sscans(WINDOW*, const char*, va_list);

/*
 * C++ interface to curses library.
 *
 */

/*
 * varargs functions are handled conservatively:
 * They interface directly into the underlying 
 * _doscan, _doprnt and/or vfprintf routines rather than
 * assume that such things are handled compatibly in the curses library
 */

int CursesWindow::scanw(const char * fmt, ...)
{
  va_list args;
  va_start(args, fmt);
#if 1 /* bsd */
  int result = _sscans(w, fmt, args);
#else
#ifdef VMS
  int result = wscanw(w , fmt , args);
#else
  char buf[BUFSIZ];
  int result = wgetstr(w, buf);
  if (result == OK) {
#ifndef HAVE_VSCANF
      FILE b;
      b._flag = _IOREAD|_IOSTRG;
      b._base = buf;
      b._ptr = buf;
      b._cnt = BUFSIZ;
      result = _doscan(&b, fmt, args);
#else
      result = vsscanf(buf, fmt, args);
#endif
#endif
  }
#endif
  va_end(args);
  return result;
}

int CursesWindow::mvscanw(int y, int x, const char * fmt, ...)
{
  va_list args;
  va_start(args, fmt);
#ifndef VMS
  char buf[BUFSIZ];
  int result = wmove(w, y, x);
  if (result == OK)
#if 1 /* bsd */
  result = _sscans(w, fmt, args);
#else
#ifdef VMS
  result=wscanw(w , fmt , args);
#else
  {
    result = wgetstr(w, buf);
    if (result == OK) {
#ifndef HAVE_VSCANF
	FILE b;
	b._flag = _IOREAD|_IOSTRG;
	b._base = buf;
	b._ptr = buf;
	b._cnt = BUFSIZ;
	result = _doscan(&b, fmt, args);
#else
	result = vsscanf(buf, fmt, args);
#endif
#endif
    }
  }
#endif
#endif
  va_end(args);
  return result;
}

int CursesWindow::printw(const char * fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  char buf[BUFSIZ];
#ifndef HAVE_VPRINTF
  FILE b;
  b._flag = _IOWRT|_IOSTRG;
  b._ptr = buf;
  b._cnt = BUFSIZ;
  _doprnt(fmt, args, &b);
  putc('\0', &b);
#else
  vsprintf(buf, fmt, args);
#endif
  va_end(args);
  return waddstr(w, buf);
}


int CursesWindow::mvprintw(int y, int x, const char * fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  char buf[BUFSIZ];
  int result = wmove(w, y, x);
  if (result == OK)
  {
#ifndef HAVE_VPRINTF
    FILE b;
    b._flag = _IOWRT|_IOSTRG;
    b._ptr = buf;
    b._cnt = BUFSIZ;
    _doprnt(fmt, args, &b);
    putc('\0', &b);
#else
    vsprintf(buf, fmt, args);
#endif
    result = waddstr(w, buf);
  }
  va_end(args);
  return result;
}

CursesWindow::CursesWindow(int lines, int cols, int begin_y, int begin_x)
{
  if (count==0)
    initscr();

  w = newwin(lines, cols, begin_y, begin_x);
  if (w == 0)
  {
    (*lib_error_handler)("CursesWindow", "Cannot construct window");
  }

  alloced = 1;
  subwins = par = sib = 0;
  count++;
}

CursesWindow::CursesWindow(WINDOW* &window)
{
  if (count==0)
    initscr();

  w = window;
  alloced = 0;
  subwins = par = sib = 0;
  count++;
}

CursesWindow::CursesWindow(CursesWindow& win, int l, int c, 
                           int by, int bx, char absrel)
{

  if (absrel == 'r') // relative origin
  {
    by += win.begy();
    bx += win.begx();
  }

  // Even though we treat subwindows as a tree, the standard curses
  // library needs the `subwin' call to link to the root in
  // order to correctly perform refreshes, etc.

  CursesWindow* root = &win;
  while (root->par != 0) root = root->par;

  w = subwin(root->w, l, c, by, bx);
  if (w == 0)
  {
    (*lib_error_handler)("CursesWindow", "Cannot construct subwindow");
  }

  par = &win;
  sib = win.subwins;
  win.subwins = this;
  subwins = 0;
  alloced = 1;
  count++;
}


void CursesWindow::kill_subwindows()
{
  for (CursesWindow* p = subwins; p != 0; p = p->sib)
  {
    p->kill_subwindows();
    if (p->alloced)
    {
      if (p->w != 0)
        ::delwin(p->w);
      p->alloced = 0;
    }
    p->w = 0; // cause a run-time error if anyone attempts to use...
  }
}
    
CursesWindow::~CursesWindow() 
{
  kill_subwindows();

  if (par != 0)   // Snip us from the parent's list of subwindows.
  {
    CursesWindow * win = par->subwins;
    CursesWindow * trail = 0;
    for (;;)
    {
      if (win == 0)
        break;
      else if (win == this)
      {
        if (trail != 0)
          trail->sib = win->sib;
        else
          par->subwins = win->sib;
        break;
      }
      else
      {
        trail = win;
        win = win->sib;
      }
    }
  }

  if (alloced && w != 0) 
    delwin(w);

  --count;
  if (count == 0) 
    endwin();
  else if (count < 0) // cannot happen!
  {
    (*lib_error_handler)("CursesWindow", "Too many windows destroyed");
  }
}
