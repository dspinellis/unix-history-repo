# include	<sccs.h>

SCCSID(@(#)clrmem.c	7.1	2/5/81)

/*
**  CLRMEM -- clear a block of memory
**
**	A block of memory is set to zero.  If we use assembly
**	language, this can be potentially very fast.
**
**	Parameters:
**		p -- a pointer to the area to be cleared.
**		l -- the length of the area in bytes.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		none
*/

clrmem(p, l)
register char	*p;
register int	l;
{
	while (l-- > 0)
		*p++ = 0;
}
