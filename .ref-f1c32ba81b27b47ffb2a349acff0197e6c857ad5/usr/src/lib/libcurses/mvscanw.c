/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mvscanw.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include "curses.ext"

/*
 * implement the mvscanw commands.  Due to the variable number of
 * arguments, they cannot be macros.  Another sigh....
 */

#if __STDC__
mvscanw(reg int y, reg int x, const char *fmt, ...)
#else
mvscanw(y, x, fmt, va_alist)
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
	ret = _sscans(stdscr, fmt, ap);
	va_end(ap);
	return ret;
}

#if __STDC__
mvwscanw(reg WINDOW *win, reg int y, reg int x, const char *fmt, ...)
#else
mvwscanw(win, y, x, fmt, va_alist)
	reg WINDOW *win;
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
	ret = _sscans(win, fmt, ap);
	va_end(ap);
	return ret;
}
