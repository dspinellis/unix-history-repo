/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mvprintw.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include "curses.ext"

/*
 * implement the mvprintw commands.  Due to the variable number of
 * arguments, they cannot be macros.  Sigh....
 */

#if __STDC__
mvprintw(reg int y, reg int x, const char *fmt, ...)
#else
mvprintw(y, x, fmt, va_alist)
	reg int y, x;
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
	int ret;

	if (move(y, x) != OK)
		return ERR;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = _sprintw(stdscr, fmt, ap);
	va_end(ap);
	return ret;
}

#if __STDC__
mvwprintw(reg WINDOW *win, reg int y, reg int x, const char *fmt, ...)
#else
mvwprintw(win, y, x, fmt, va_alist)
	reg WINDOW *win;
	reg int y, x;
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
	int ret;

	if (wmove(win, y, x) != OK)
		return ERR;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = _sprintw(win, ap);
	va_end(ap);
	return ret;
}
