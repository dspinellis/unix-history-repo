# include	<sccs.h>

SCCSID(@(#)IIconcatv.c	7.1	2/5/81)


/*
**  IIconcatv -- concatenate strings into a buffer.
**
**	Parameters:
**		buf -- result buffer
**		args -- first of a 0 terminated series of string pointers.
**	
**	Returns:
**		a pointer to the null byte appended.
**
*/


char	*IIconcatv(buf, args)
char	*buf;
char	*args;
{
	register char	*p;
	register char	**i;
	register char	*s;

	i = &args;
	p = buf;
	while (s = *i++)
	{
		/* move the current arg over */
		while (*p = *s++)
			p++;
	}
	*p = '\0';

	return (p);
}
