# include	"curses.ext"

/*
 * implement the mvprintw commands.  Due to the variable number of
 * arguments, they cannot be macros.  Sigh....
 *
 * 1/26/81 (Berkeley) @(#)mvprintw.c	1.1
 */

mvprintw(y, x, fmt, args)
reg int		y, x;
char		*fmt;
int		args; {

	return move(y, x) == OK ? _sprintw(stdscr, fmt, &args) : ERR;
}

mvwprintw(win, y, x, fmt, args)
reg WINDOW	*win;
reg int		y, x;
char		*fmt;
int		args; {

	return wmove(win, y, x) == OK ? _sprintw(win, fmt, &args) : ERR;
}
