# include	<sccs.h>

SCCSID(@(#)atof.c	7.1	2/5/81)

/*
**  ATOF -- ASCII TO FLOATING CONVERSION
**
**	Converts the string 'str' to floating point and stores the
**	result into the cell pointed to by 'val'.
**
**	The syntax which it accepts is pretty much what you would
**	expect.  Basically, it is:
**		{<sp>} [+|-] {<sp>} {<digit>} [.{digit}] {<sp>} [<exp>]
**	where <exp> is "e" or "E" followed by an integer, <sp> is a
**	space character, <digit> is zero through nine, [] is zero or
**	one, and {} is zero or more.
**
**	Parameters:
**		str -- string to convert.
**		val -- pointer to place to put the result (which
**			must be type double).
**
**	Returns:
**		zero -- ok.
**		-1 -- syntax error.
**		+1 -- overflow (someday).
**
**	Side Effects:
**		clobbers *val.
*/

atof(str, val)
char	*str;
double	*val;
{
	register char	*p;
	double		v;
	extern double	pow();
	double		fact;
	int		minus;
	register char	c;
	int		expon;
	register int	gotmant;

	v = 0.0;
	p = str;
	minus = 0;

	/* skip leading blanks */
	while (c = *p)
	{
		if (c != ' ')
			break;
		p++;
	}

	/* handle possible sign */
	switch (c)
	{

	  case '-':
		minus++;

	  case '+':
		p++;

	}

	/* skip blanks after sign */
	while (c = *p)
	{
		if (c != ' ')
			break;
		p++;
	}

	/* start collecting the number to the decimal point */
	gotmant = 0;
	for (;;)
	{
		c = *p;
		if (c < '0' || c > '9')
			break;
		v = v * 10.0 + (c - '0');
		gotmant++;
		p++;
	}

	/* check for fractional part */
	if (c == '.')
	{
		fact = 1.0;
		for (;;)
		{
			c = *++p;
			if (c < '0' || c > '9')
				break;
			fact *= 0.1;
			v += (c - '0') * fact;
			gotmant++;
		}
	}

	/* skip blanks before possible exponent */
	while (c = *p)
	{
		if (c != ' ')
			break;
		p++;
	}

	/* test for exponent */
	if (c == 'e' || c == 'E')
	{
		p++;
		if (atoi(p, &expon))
			return (-1);
		if (!gotmant)
			v = 1.0;
		fact = expon;
		v *= pow(10.0, fact);
	}
	else
	{
		/* if no exponent, then nothing */
		if (c != 0)
			return (-1);
	}

	/* store the result and exit */
	if (minus)
		v = -v;
	*val = v;
	return (0);
}
