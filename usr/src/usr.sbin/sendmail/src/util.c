# include <sysexits.h>
# include "useful.h"

/*
**  UTIL.C -- General Purpose Routines
**
**	Defines:
**		stripquotes
**		xalloc
**		any
*/
/*
**  STRIPQUOTES -- Strip quotes & quote bits from a string.
**
**	Runs through a string and strips off unquoted quote
**	characters and quote bits.  This is done in place.
**
**	Parameters:
**		s -- the string to strip.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
**
**	Requires:
**		none.
**
**	Called By:
**		deliver
**
**	History:
**		3/5/80 -- written.
*/

stripquotes(s)
	char *s;
{
	register char *p;
	register char *q;
	register char c;

	for (p = q = s; (c = *p++) != '\0'; )
	{
		if (c != '"')
			*q++ = c & 0177;
	}
	*q = '\0';
}
/*
**  XALLOC -- Allocate memory and bitch wildly on failure.
**
**	THIS IS A CLUDGE.  This should be made to give a proper
**	error -- but after all, what can we do?
**
**	Parameters:
**		sz -- size of area to allocate.
**
**	Returns:
**		pointer to data region.
**
**	Side Effects:
**		Memory is allocated.
**
**	Requires:
**		malloc
**		syserr
**		exit
**
**	Called By:
**		lots of people.
**
**	History:
**		12/31/79 -- written.
*/

char *
xalloc(sz)
	register unsigned int sz;
{
	register char *p;
	extern char *malloc();

	p = malloc(sz);
	if (p == NULL)
	{
		syserr("Out of memory!!");
		exit(EX_UNAVAIL);
	}
	return (p);
}
/*
**  ANY -- Return TRUE if the character exists in the string.
**
**	Parameters:
**		c -- the character.
**		s -- the string
**			(sounds like an avant garde script)
**
**	Returns:
**		TRUE -- if c could be found in s.
**		FALSE -- otherwise.
**
**	Side Effects:
**		none.
**
**	Requires:
**		none.
**
**	Called By:
**		prescan
**
**	History:
**		3/5/80 -- written.
*/

any(c, s)
	register char c;
	register char *s;
{
	register char c2;

	while ((c2 = *s++) != '\0')
		if (c2 == c)
			return (TRUE);
	return (FALSE);
}
