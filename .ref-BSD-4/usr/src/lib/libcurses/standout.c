# include	"curses.ext"

/*
 * enter standout mode
 */
char *
wstandout(win)
reg WINDOW	*win;
{
	if (!SO)
		return FALSE;

	win->_flags |= _STANDOUT;
	return SO;
}

/*
 * exit standout mode
 */
char *
wstandend(win)
reg WINDOW	*win;
{
	if (!SO)
		return FALSE;

	win->_flags &= ~_STANDOUT;
	return SE;
}
