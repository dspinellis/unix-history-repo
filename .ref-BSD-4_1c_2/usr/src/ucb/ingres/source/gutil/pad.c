# include	<sccs.h>

SCCSID(@(#)pad.c	7.1	2/5/81)

/*
**  PAD STRING OUT WITH BLANKS
**
**	This routine is an in-place pmove which always pads
**	with blanks.
*/

pad(s, n)
char	*s;
int	n;
{
	register char	*ss;

	ss = s;
	pmove(ss, ss, n, ' ');
}
