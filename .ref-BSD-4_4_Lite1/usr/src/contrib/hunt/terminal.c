/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 */

# include	"hunt.h"
# define	TERM_WIDTH	80	/* Assume terminals are 80-char wide */

/*
 * cgoto:
 *	Move the cursor to the given position on the given player's
 *	terminal.
 */
cgoto(pp, y, x)
register PLAYER	*pp;
register int	y, x;
{
	if (x == pp->p_curx && y == pp->p_cury)
		return;
	sendcom(pp, MOVE, y, x);
	pp->p_cury = y;
	pp->p_curx = x;
}

/*
 * outch:
 *	Put out a single character.
 */
outch(pp, ch)
register PLAYER	*pp;
char		ch;
{
	if (++pp->p_curx >= TERM_WIDTH) {
		pp->p_curx = 0;
		pp->p_cury++;
	}
	(void) putc(ch, pp->p_output);
}

/*
 * outstr:
 *	Put out a string of the given length.
 */
outstr(pp, str, len)
register PLAYER	*pp;
register char	*str;
register int	len;
{
	pp->p_curx += len;
	pp->p_cury += (pp->p_curx / TERM_WIDTH);
	pp->p_curx %= TERM_WIDTH;
	while (len--)
		(void) putc(*str++, pp->p_output);
}

/*
 * clrscr:
 *	Clear the screen, and reset the current position on the screen.
 */
clrscr(pp)
register PLAYER	*pp;
{
	sendcom(pp, CLEAR);
	pp->p_cury = 0;
	pp->p_curx = 0;
}

/*
 * ce:
 *	Clear to the end of the line
 */
ce(pp)
PLAYER	*pp;
{
	sendcom(pp, CLRTOEOL);
}

/*
 * ref;
 *	Refresh the screen
 */
ref(pp)
register PLAYER	*pp;
{
	sendcom(pp, REFRESH);
}

/*
 * sendcom:
 *	Send a command to the given user
 */
/* VARARGS2 */
sendcom(pp, command, arg1, arg2)
register PLAYER		*pp;
register int	command;
int			arg1, arg2;
{
	(void) putc(command, pp->p_output);
	switch (command & 0377) {
	  case MOVE:
		(void) putc(arg1, pp->p_output);
		(void) putc(arg2, pp->p_output);
		break;
	  case ADDCH:
	  case READY:
		(void) putc(arg1, pp->p_output);
		break;
	}
}
