/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)printw.c	5.9 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/*
 * printw and friends.
 *
 * These routines make nonportable assumptions about varargs if __STDC__
 * is not in effect.
 */

static int __winwrite __P((void *, const char *, int));

/*
 * printw --
 *	Printf on the standard screen.
 */
#if __STDC__
printw(const char *fmt, ...)
#else
printw(fmt, va_alist)
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
	ret = _sprintw(stdscr, fmt, ap);
	va_end(ap);
	return (ret);
}

/*
 * wprintw --
 *	Printf on the given window.
 */
#if __STDC__
wprintw(WINDOW * win, const char *fmt, ...)
#else
wprintw(win, fmt, va_alist)
	WINDOW *win;
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
	int ret;

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
 * Internal write-buffer-to-window function.
 */
static int
__winwrite(cookie, buf, n)
	void *cookie;
	register const char *buf;
	int n;
{
	register WINDOW *win;
	register int c;

	for (c = n, win = cookie; --c >= 0;)
		if (waddch(win, *buf++) == ERR)
			return (-1);
	return (n);
}

/*
 * __sprintw --
 *	This routine actually executes the printf and adds it to the window.
 *	It must not be declared static as it is used in mvprintw.c.
 *	THIS SHOULD BE RENAMED vwprintw AND EXPORTED
 */
__sprintw(win, fmt, ap)
	WINDOW *win;
#if __STDC__
	const char *fmt;
#else
	char *fmt;
#endif
	va_list ap;
{
	FILE *f;

	if ((f = funopen(win, NULL, __winwrite, NULL, NULL)) == NULL)
		return (ERR);
	(void)vfprintf(f, fmt, ap);
	return (fclose(f) ? ERR : OK);
}
