# include	<sccs.h>

SCCSID(@(#)IIitos.c	7.1	2/5/81)


/*
**  INTEGER OUTPUT CONVERSION
**
**	The integer `i' is converted to ascii and put
**	into the static buffer `buf'.  The address of the starting
**	point in `buf' is
**	returned.
**
**	Number is converted from least significant forwards.
*/

char *IIitos(i1)
int	i1;
{
	register char	*a;
	register int	i;
	static char	buf[25];

	i = i1;
	if (i < 0)
		i = -i;

	a = &buf[sizeof buf - 1];
	*a-- = '\0';
	do
	{
		*a-- = i % 10 + '0';
		i /= 10;
	} while (i);
	if (i1 < 0)
		*a-- = '-';

	a++;
	return (a);
}
