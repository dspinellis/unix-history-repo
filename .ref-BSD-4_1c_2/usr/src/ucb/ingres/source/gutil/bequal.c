# include	<useful.h>
# include	<sccs.h>

SCCSID(@(#)bequal.c	7.1	2/5/81)

/*
**  BLOCK EQUALITY TEST
**
**	blocks `a' and `b', both of length `l', are tested
**		for absolute equality.
**	Returns one for equal, zero otherwise.
*/

bool
bequal(a, b, l)
register char	*a, *b;
register int	l;
{
	while (l-- > 0)
		if (*a++ != *b++)
			return(FALSE);
	return(TRUE);
}
