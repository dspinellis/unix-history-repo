# include	<sccs.h>

SCCSID(@(#)smove.c	7.1	2/5/81)

/*
**  STRING MOVE
**
**	The string `a' is moved to the string `b'.  The length
**	of the string is returned.  `a' must be null terminated.
**	There is no test for overflow of `b'.
*/

smove(a, b)
register char	*a, *b;
{
	register int	l;

	l = 0;
	while (*a != '\0')
	{
		*b++ = *a++;
		l++;
	}
	*b = '\0';
	return (l);
}
