# include	<useful.h>
# include	<sccs.h>

SCCSID(@(#)sequal.c	7.1	2/5/81)

/*
**  SEQUAL -- string equality test
**
**	null-terminated strings `a' and `b' are tested for
**		absolute equality.
**	returns one if equal, zero otherwise.
*/

bool
sequal(a, b)
register char	*a, *b;
{
	while (*a || *b)
		if (*a++ != *b++)
			return(FALSE);
	return(TRUE);
}
