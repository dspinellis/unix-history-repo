/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)mvprintw.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"curses.ext"

/*
 * implement the mvprintw commands.  Due to the variable number of
 * arguments, they cannot be macros.  Sigh....
 *
 */

mvprintw(y, x, fmt, args)
reg int		y, x;
char		*fmt;
int		args; {

	char	buf[512];

	if (move(y, x) != OK)
		return ERR;
	(void) vsprintf(buf, fmt, &args);
	return waddstr(stdscr, buf);
}

mvwprintw(win, y, x, fmt, args)
reg WINDOW	*win;
reg int		y, x;
char		*fmt;
int		args; {

	char	buf[512];

	if (move(y, x) != OK)
		return ERR;
	(void) vsprintf(buf, fmt, &args);
	return waddstr(win, buf);
}
