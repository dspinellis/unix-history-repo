# include	<sccs.h>

SCCSID(@(#)append.c	7.1	2/5/81)

/*
**  APPEND -- block concatenate
**
**	block `b1' of length `l1' is concatenated to block
**	`b2' of length `l2', giving `b3'.
**
**	Returns the address of the next byte available after
**	the end of `b3'.
*/

char *
append(b1, l1, b2, l2, b3)
int	l1, l2;
char	*b1, *b2, *b3;
{
	register char	*p, *q;
	register int	n;

	p = b3;
	n = l1;
	q = b1;
	while (n-- > 0)
		*p++ = *q++;
	n = l2;
	q = b2;
	while (n-- > 0)
		*p++ = *q++;
	return (p);
}
