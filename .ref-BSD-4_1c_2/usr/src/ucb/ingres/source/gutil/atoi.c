# include	<ingres.h>
# include	<sccs.h>

SCCSID(@(#)atoi.c	7.1	2/5/81)

/*
**  ASCII CHARACTER STRING TO INTEGER CONVERSION
**
**	`a' is a pointer to the character string, `i' is a
**	pointer to the word which is to contain the result.
**	The integer is either 16-bit or 32-bit depending upon
**	the natural word length of the machine.
**
**	The return value of the function is:
**		zero:	succesful conversion; `i' contains the integer
**		+1:	numeric overflow; `i' is unchanged
**		-1:	syntax error; `i' is unchanged
**
**	A valid string is of the form:
**		<space>* [+-] <space>* <digit>* <space>*
*/

atoi(a, i)
register char	*a;
int	*i;
{
# ifdef PDP11
	int		sign;		/* flag to indicate the sign */
	register int	x;		/* holds the integer being formed */
	register char	c;

	sign = 0;
	/* skip leading blanks */
	while (*a == ' ')
		a++;
	/* check for sign */
	switch (*a)
	{

	  case '-':
		sign = -1;

	  case '+':
		while (*++a == ' ');
	}

	/* at this point everything had better be numeric */
	x = 0;
	while ((c = *a) <= '9' && c >= '0')
	{
		if (x > 3276 || (x == 3276 && c > '7'))
			return (1);		/* overflow */
		x = x * 10 + c - '0';
		a++;
	}

	/* eaten all the numerics; better be all blanks */
	while (c = *a++)
		if(c != ' ')			/* syntax error */
			return (-1);
	*i = sign ? -x : x;
	return (0);		/* successful termination */
# else
	return (atol(a, i));
# endif
}
