# include	"curses.ext"

/*
 *	This routine gets a string starting at (_cury,_curx)
 *
 * %G% (Berkeley) @(#)getstr.c	1.1
 */
wgetstr(win,str)
reg WINDOW	*win; 
reg char	*str; {

	while ((*str = wgetch(win)) != ERR && *str != '\n');
		str++;
	*str = '\0';
	if (*str == ERR)
		return ERR;
	return OK;
}
