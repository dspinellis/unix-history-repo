# include	<sccs.h>

SCCSID(@(#)itoa.c	7.1	2/5/81)

/*
**  ITOA -- integer to ascii conversion
*/

itoa(i, a)
register int	i;
register char	*a;
{
	register char	*j;
	char		b[6];

	if (i < 0)
	{
		*a++ = '-';
		i = -i;
	}
	j = &b[5];
	*j-- = 0;
	do
	{
		*j-- = i % 10 + '0';
		i /= 10;
	} while (i);
	do
	{
		*a++ = *++j;
	} while (*j);
	return (0);
}
