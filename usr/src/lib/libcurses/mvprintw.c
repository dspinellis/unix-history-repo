/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mvprintw.c	5.6 (Berkeley) %G%";
#endif /* not lint */

# include	<varargs.h>
# include	"curses.ext"

/*
 * implement the mvprintw commands.  Due to the variable number of
 * arguments, they cannot be macros.  Sigh....
 *
 */

mvprintw(va_alist)
va_dcl {

	va_list	ap;
	reg int	y, x;
	int	ret;

	va_start(ap);
	y = va_arg(ap, int);
	x = va_arg(ap, int);
	if (move(y, x) != OK)
		ret = ERR;
	else
		ret = _sprintw(stdscr, ap);
	va_end(ap);
	return ret;
}

mvwprintw(va_alist)
va_dcl {

	va_list		ap;
	reg WINDOW	*win;
	reg int		y, x;
	int		ret;

	va_start(ap);
	win = va_arg(ap, WINDOW *);
	y = va_arg(ap, int);
	x = va_arg(ap, int);
	if (wmove(win, y, x) != OK)
		ret = ERR;
	else
		ret = _sprintw(win, ap);
	va_end(ap);
	return ret;
}
