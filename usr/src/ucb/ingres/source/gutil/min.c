# include	<sccs.h>

SCCSID(@(#)min.c	7.1	2/5/81)

/*
**  MIN -- return the minimum of two integers.
**
**	Why should I even have to write this????
*/

min(a, b)
int	a;
int	b;
{
	return ((a < b) ? a : b);
}
