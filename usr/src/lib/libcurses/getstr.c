# include	"curses.ext"

/*
 *	This routine gets a string starting at (_cury,_curx)
 *
 * @(#)getstr.c	1.5 (Berkeley) %G%
 */
wgetstr(win,str)
reg WINDOW	*win; 
reg char	*str; {

	while ((*str = wgetch(win)) != ERR && *str != '\n')
		str++;
	if (*str == ERR) {
		*str = '\0';
		return ERR;
	}
	*str = '\0';
	return OK;
}
