# include	<sccs.h>

SCCSID(@(#)oatoi.c	7.1	2/5/81)

/*
**  OCTAL ASCII TO INTEGER CONVERSION
**
**	The ascii string 'a' which represents an octal number
**	is converted to binary and returned.  Conversion stops at any
**	non-octal digit. The integer reflects the natural word length of the
**	machine.
**
**	Note that the number may not have a sign, and may not have
**	leading blanks.
**
**	(Intended for converting the status codes in users(FILE))
*/

oatoi(a)
char	*a;
{
	register int	r;
	register char	*p;
	register char	c;

	r = 0;
	p = a;

	while ((c = *p++) >= '0' && c <= '7')
		r = (r << 3) | (c &= 7);

	return (r);
}
