/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)scanw.c	5.7 (Berkeley) %G%";
#endif /* not lint */

/*
 * scanw and friends
 *
 */

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include "curses.ext"

/*
 *	This routine implements a scanf on the standard screen.
 */
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
	ret = _sscans(stdscr, fmt, ap);
	va_end(ap);
	return ret;
}

/*
 *	This routine implements a scanf on the given window.
 */
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
	ret = _sscans(win, fmt, ap);
	va_end(ap);
	return ret;
}

/*
 *	This routine actually executes the scanf from the window.
 *	THIS SHOULD BE RENAMED vwscanw AND EXPORTED
 */
_sscans(win, fmt, ap)
	WINDOW *win;
#if __STDC__
	const char *fmt;
#else
	char *fmt;
#endif
	va_list ap;
{
	char buf[100];

	return wgetstr(win, buf) == OK ? vsscanf(buf, fmt, ap) : ERR;
}
