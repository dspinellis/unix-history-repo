# include	<sccs.h>

SCCSID(@(#)IIsequal.c	7.1	2/5/81)


/*
**  IISEQUAL -- String equality comparison
**
**	Parameters:
**		s1, s2 -- strings to be tested for absolute equality
**
**	Returns:
**		1 -- if =
**		0 otherwise
**
**	Side Effects:
**		none
**
**	Called By:
**		IIgetpath() [IIingres.c]
*/

IIsequal(s1, s2)
char	*s1, *s2;
{
	register char	*r1, *r2;


	r1 = s1;
	r2 = s2;
	while (*r1 || *r2)
		if (*r1++ != *r2++)
			return (0);
	return (1);
}
