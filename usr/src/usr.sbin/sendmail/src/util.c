# include <stdio.h>
# include <sysexits.h>
# include "useful.h"
# include <ctype.h>

static char	SccsId[] = "@(#)util.c	3.6	%G%";

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

	p = xalloc((unsigned) (strlen(s) + 1));
	strcpy(p, s);
	return (p);
}
/*
**  COPYPLIST -- copy list of pointers.
**
**	This routine is the equivalent of newstr for lists of
**	pointers.
**
**	Parameters:
**		list -- list of pointers to copy.
**			Must be NULL terminated.
**		copycont -- if TRUE, copy the contents of the vector
**			(which must be a string) also.
**
**	Returns:
**		a copy of 'list'.
**
**	Side Effects:
**		none.
*/

char **
copyplist(list, copycont)
	char **list;
	bool copycont;
{
	register char **vp;
	register char **newvp;

	for (vp = list; *vp != NULL; vp++)
		continue;

	vp++;

	newvp = (char **) xalloc((unsigned) (vp - list) * sizeof *vp);
	bmove((char *) list, (char *) newvp, (vp - list) * sizeof *vp);

	if (copycont)
	{
		for (vp = newvp; *vp != NULL; vp++)
			*vp = newstr(*vp);
	}

	return (newvp);
}
/*
**  PRINTAV -- print argument vector.
**
**	Parameters:
**		av -- argument vector.
**
**	Returns:
**		none.
**
**	Side Effects:
**		prints av.
*/

# ifdef DEBUG
printav(av)
	register char **av;
{
	while (*av != NULL)
	{
		printf("\t%08x=", *av);
		xputs(*av++);
		putchar('\n');
	}
}
# endif DEBUG
/*
**  LOWER -- turn letter into lower case.
**
**	Parameters:
**		c -- character to turn into lower case.
**
**	Returns:
**		c, in lower case.
**
**	Side Effects:
**		none.
*/

char
lower(c)
	register char c;
{
	if (isascii(c) && isupper(c))
		c = c - 'A' + 'a';
	return (c);
}
/*
**  XPUTS -- put string doing control escapes.
**
**	Parameters:
**		s -- string to put.
**
**	Returns:
**		none.
**
**	Side Effects:
**		output to stdout
*/

# ifdef DEBUG
xputs(s)
	register char *s;
{
	register char c;

	while ((c = *s++) != '\0')
	{
		if (!isascii(c))
		{
			putchar('\\');
			c &= 0177;
		}
		if (iscntrl(c))
		{
			putchar('^');
			c |= 0100;
		}
		putchar(c);
	}
	(void) fflush(stdout);
}
# endif DEBUG
/*
**  MAKELOWER -- Translate a line into lower case
**
**	Parameters:
**		p -- the string to translate.  If NULL, return is
**			immediate.
**
**	Returns:
**		none.
**
**	Side Effects:
**		String pointed to by p is translated to lower case.
**
**	Called By:
**		parse
*/

makelower(p)
	register char *p;
{
	register char c;

	if (p == NULL)
		return;
	for (; (c = *p) != '\0'; p++)
		if (isascii(c) && isupper(c))
			*p = c - 'A' + 'a';
}
/*
**  SAMEWORD -- return TRUE if the words are the same
**
**	Ignores case.
**
**	Parameters:
**		a, b -- the words to compare.
**
**	Returns:
**		TRUE if a & b match exactly (modulo case)
**		FALSE otherwise.
**
**	Side Effects:
**		none.
*/

bool
sameword(a, b)
	register char *a, *b;
{
	while (lower(*a) == lower(*b))
	{
		if (*a == '\0')
			return (TRUE);
		a++;
		b++;
	}
	return (FALSE);
}
/*
**  SYSLOG -- fake entry to fool lint
*/

# ifdef LOG
# ifdef lint

/*VARARGS2*/
syslog(pri, fmt, args)
	int pri;
	char *fmt;
{
	pri = *fmt;
	args = pri;
	pri = args;
}

# endif lint
# endif LOG
