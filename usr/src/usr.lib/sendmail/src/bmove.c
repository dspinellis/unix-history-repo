# include "useful.h"

SCCSID(@(#)bmove.c	4.1		7/25/83);

/*
**  BMOVE -- block move.
**
**	Parameters:
**		s -- source of bytes.
**		d -- destination of bytes.
**		l -- length of block to move.
**
**	Returns:
**		none.
**
**	Side Effects:
**		copies 's' to 'd' for 'l' bytes.
**
**	Notes:
**		This can be easily written in assembly language on
**		machines like VAXes if performance is needed.
*/

/*VARARGS0*/
bmove(s, d, l)
	register char *s, *d;
	register int l;
{
	while (l-- > 0)
		*d++ = *s++;
}
