# include	<sccs.h>

SCCSID(@(#)getufield.c	7.1	2/5/81)

/*
**  GETUFIELD -- extract field from users file
**
**	A buffer returned as the result of a getuser() (or getpw())
**	call is scanned for the indicated parameter, numbered from
**	zero.  A pointer to the parameter is returned.
*/

char *
getufield(buf, num)
char	*buf;
int	num;
{
	register char	c;
	register int	i;
	register char	*p;
	char		*r;

	p = buf;

	/* skip other fields */
	for (i = num; i > 0; i--)
	{
		while ((c = *p++) != 0)
			if (c == ':')
				break;
	}

	/* save result pointer */
	r = p;

	/* null-terminate this field */
	while ((c = *p++) != 0)
		if (c == ':')
			break;

	*--p = 0;

	return (r);
}
