# include	<sccs.h>

SCCSID(@(#)pmove.c	7.1	2/5/81)

/*
**  Packed Move
**
**	Moves string `s' to storage area `b' of length `l' bytes.  If
**	`s' is too long, it is truncated, otherwise it is padded to
**	length `l' with character `c'.  `B' after the transfer is
**	returned.
*/

char *
pmove(s1, b1, l1, c)
char	*s1;
char	*b1;
int	l1;
char	c;
{
	register char	*s;
	register char	*b;
	register int	l;

	s = s1;
	b = b1;
	l = l1;

	/* move up to `l' bytes */
	while (*s && l > 0)
	{
		*b++ = *s++;
		l -= 1;
	}

	/* if we still have some `l', pad */
	while (l-- > 0)
	{
		*b++ = c;
	}

	return (b);
}
