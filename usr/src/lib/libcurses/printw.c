/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)printw.c	5.8 (Berkeley) %G%";
#endif /* not lint */

/*
 * printw and friends.
 *
 * These routines make nonportable assumptions about varargs if __STDC__
 * is not in effect.
 */

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include "curses.ext"

/*
 *	This routine implements a printf on the standard screen.
 */
#if __STDC__
printw(const char *fmt, ...)
#else
printw(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	va_list	ap;
	int	ret;

#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = _sprintw(stdscr, fmt, ap);
	va_end(ap);
	return (ret);
}

/*
 *	This routine implements a printf on the given window.
 */
#if __STDC__
wprintw(WINDOW *win, const char *fmt, ...)
#else
wprintw(win, fmt, va_alist)
	WINDOW *win;
	char *fmt;
	va_dcl
#endif
{
	va_list	ap;
	int	ret;

#ifdef __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = _sprintw(win, fmt, ap);
	va_end(ap);
	return (ret);
}

/*
 *	Internal write-buffer-to-window function.
 */
static int
_winwrite(cookie, buf, n)
	void *cookie;
	register char *buf;
	int n;
{
	register WINDOW *win = (WINDOW *)cookie;
	register int c = n;

	while (--c >= 0) {
		if (waddch(win, *buf++) == ERR)
			return (-1);
	}
	return n;
}

/*
 *	This routine actually executes the printf and adds it to the window.
 *	It must not be declared static as it is used in mvprintw.c.
 *	THIS SHOULD BE RENAMED vwprintw AND EXPORTED
 */
_sprintw(win, fmt, ap)
	WINDOW *win;
#if __STDC__
	const char *fmt;
#else
	char *fmt;
#endif
	va_list	ap;
{
	FILE *f;

	if ((f = fwopen((void *)win, _winwrite)) == NULL)
		return ERR;
	(void) vfprintf(f, fmt, ap);
	return fclose(f) ? ERR : OK;
}
