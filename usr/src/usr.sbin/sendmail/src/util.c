# include <sysexits.h>
# include "useful.h"
# include <ctype.h>

static char	SccsId[] = "@(#)util.c	3.1	%G%";

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
**	Called By:
**		deliver
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
**  CAPITALIZE -- return a copy of a string, properly capitalized.
**
**	Parameters:
**		s -- the string to capitalize.
**
**	Returns:
**		a pointer to a properly capitalized string.
**
**	Side Effects:
**		none.
*/

char *
capitalize(s)
	register char *s;
{
	static char buf[50];
	register char *p;

	p = buf;

	for (;;)
	{
		while (!isalpha(*s) && *s != '\0')
			*p++ = *s++;
		if (*s == '\0')
			break;
		*p++ = toupper(*s++);
		while (isalpha(*s))
			*p++ = *s++;
	}

	*p = '\0';
	return (buf);
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
		exit(EX_UNAVAILABLE);
	}
	return (p);
}
/*
**  NEWSTR -- make copy of string.
**
**	Space is allocated for it using xalloc.
**
**	Parameters:
**		string to copy.
**
**	Returns:
**		pointer to new string.
**
**	Side Effects:
**		none.
*/

char *
newstr(s)
	register char *s;
{
	register char *p;

	p = xalloc(strlen(s) + 1);
	strcpy(p, s);
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
**	Called By:
**		prescan
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
