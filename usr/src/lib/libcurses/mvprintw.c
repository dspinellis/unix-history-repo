/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mvprintw.c	5.9 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/*
 * mvprintw, mvwprintw --
 *	Implement the mvprintw commands.  Due to the variable number of
 *	arguments, they cannot be macros.  Sigh....
 */

#if __STDC__
mvprintw(register int y, register int x, const char *fmt, ...)
#else
mvprintw(y, x, fmt, va_alist)
	register int y, x;
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
	int ret;

	if (move(y, x) != OK)
		return (ERR);
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = __sprintw(stdscr, fmt, ap);
	va_end(ap);
	return (ret);
}

#if __STDC__
mvwprintw(register WINDOW * win, register int y, register int x,
    const char *fmt, ...)
#else
mvwprintw(win, y, x, fmt, va_alist)
	register WINDOW *win;
	register int y, x;
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
	int ret;

	if (wmove(win, y, x) != OK)
		return (ERR);
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = _sprintw(win, fmt, ap);
	va_end(ap);
	return (ret);
}
