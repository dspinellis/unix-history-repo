# include	<sccs.h>

SCCSID(@(#)IIatoi.c	7.1	2/5/81)


/*
**  IIatoi()
**  ASCII CHARACTER STRING TO 16-BIT INTEGER CONVERSION
**
**	`a' is a pointer to the character string, `i' is a
**	pointer to the word which is to contain the result.
**
**	The return value of the function is:
**		zero:	succesful conversion; `i' contains the integer
**		+1:	numeric overflow; `i' is unchanged
**		-1:	syntax error; `i' is unchanged
**
**	A valid string is of the form:
**		<space>* [+-] <space>* <digit>* <space>*
**
**		Eric's utility routine.
**
*/

IIatoi(a1, i)
char	*a1;
int	*i;
{
	int		sign;		/* flag to indicate the sign */
	register int	x;		/* holds the integer being formed */
	register char	c;
	register char	*a;

	a = a1;
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
}
