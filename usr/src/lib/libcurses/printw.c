/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)printw.c	5.7 (Berkeley) %G%";
#endif /* not lint */

/*
 * printw and friends
 *
 */

# include	<varargs.h>
# include	"curses.ext"

/*
 *	This routine implements a printf on the standard screen.
 */
printw(va_alist)
va_dcl {

	va_list	ap;
	int	ret;

	va_start(ap);
	ret = _sprintw(stdscr, ap);
	va_end(ap);
	return (ret);
}

/*
 *	This routine implements a printf on the given window.
 */
wprintw(va_alist)
va_dcl {

	va_list	ap;
	WINDOW	*win;
	int	ret;

	va_start(ap);
	win = va_arg(ap, WINDOW *);
	ret = _sprintw(win, ap);
	va_end(ap);
	return (ret);
}

/*
 *	Internal write-buffer-to-window function.
 */
static int
_winwrite(cookie, buf, n)
void	*cookie;
reg char *buf;
int	n; {

	reg WINDOW *win = (WINDOW *)cookie;
	reg int c = n;

	while (--c >= 0) {
		if (waddch(win, *buf++) == ERR)
			return (-1);
	}
	return n;
}

/*
 *	This routine actually executes the printf and adds it to the window.
 *	It must not be declared static as it is used in mvprintw.c.
 */
_sprintw(win, ap)
WINDOW	*win;
va_list	ap; {

	FILE	*f;
	char	*fmt;

	if ((f = fwopen((void *)win, _winwrite)) == NULL)
		return ERR;
	fmt = va_arg(ap, char *);
	(void) vfprintf(f, fmt, ap);
	return fclose(f) ? ERR : OK;
}
