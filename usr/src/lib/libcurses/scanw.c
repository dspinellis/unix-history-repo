/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)scanw.c	5.10 (Berkeley) %G%";
#endif	/* not lint */

/*
 * scanw and friends.
 */

#include <curses.h>

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

static int __sscans __P((WINDOW *, const char *, va_list));

/*
 * scanw --
 *	Implement a scanf on the standard screen.
 */
int
#if __STDC__
scanw(const char *fmt, ...)
#else
scanw(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
	int ret;

#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = __sscans(stdscr, fmt, ap);
	va_end(ap);
	return (ret);
}

/*
 * wscanw --
 *	Implements a scanf on the given window.
 */
int
#if __STDC__
wscanw(WINDOW *win, const char *fmt, ...)
#else
wscanw(win, fmt, va_alist)
	WINDOW *win;
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
	int ret;

#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = __sscans(win, fmt, ap);
	va_end(ap);
	return (ret);
}

/*
 * mvscanw, mvwscanw -- 
 *	Implement the mvscanw commands.  Due to the variable number of
 *	arguments, they cannot be macros.  Another sigh....
 */
int
#if __STDC__
mvscanw(register int y, register int x, const char *fmt,...)
#else
mvscanw(y, x, fmt, va_alist)
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
	ret = __sscans(stdscr, fmt, ap);
	va_end(ap);
	return (ret);
}

int
#if __STDC__
mvwscanw(register WINDOW * win, register int y, register int x,
    const char *fmt, ...)
#else
mvwscanw(win, y, x, fmt, va_alist)
	register WINDOW *win;
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
	ret = __sscans(win, fmt, ap);
	va_end(ap);
	return (ret);
}

/*
 * __sscans --
 *	This routine actually executes the scanf from the window.
 *	THIS SHOULD BE RENAMED vwscanw AND EXPORTED
 */
static int
__sscans(win, fmt, ap)
	WINDOW *win;
	const char *fmt;
	va_list ap;
{

	char buf[1024];

	return (wgetstr(win, buf) == OK ? vsscanf(buf, fmt, ap) : ERR);
}
