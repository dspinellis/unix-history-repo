# include	"curses.ext"

/*
 * get a capability from the termcap entry
 *
 * %W% (Berkeley) %G%
 */
char *
getcap(name)
char *name;
{
