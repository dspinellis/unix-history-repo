# include	<sccs.h>

SCCSID(@(#)bitcnt.c	7.1	2/5/81)


/*
**	Count the number of 1's in the integer var. As long
**	as left shift is zero fill this routine is machine
**	independent.
*/

bitcnt(var)
register int	var;
{
	register int	i, ret;

	for (ret = 0, i = 1; i; i <<= 1)
		if (i & var)
			ret++;

	return (ret);
}
