# include	"curses.ext"

/*
 *	This routine gets a string starting at (_cury,_curx)
 *
 * 7/2/81 (Berkeley) @(#)getstr.c	1.4
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
