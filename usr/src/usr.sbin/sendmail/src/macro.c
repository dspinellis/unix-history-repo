# include "useful.h"

static char SccsId[] = "@(#)macro.c	1.1	%G%";

char	*Macro[128];
extern bool	Debug;

/*
**  EXPAND -- macro expand a string using $x escapes.
**
**	Parameters:
**		s -- the string to expand.
**		buf -- the place to put the expansion.
**		buflim -- the buffer limit, i.e., the address
**			of the last usable position in buf.
**
**	Returns:
**		buf.
**
**	Side Effects:
**		none.
*/

char *
expand(s, buf, buflim)
	register char *s;
	register char *buf;
	char *buflim;
{
	register char *q;
	register char *bp;

# ifdef DEBUG
	if (Debug)
		printf("expand(%s)\n", s);
# endif DEBUG

	for (bp = buf; *s != '\0'; s++)
	{
		/* q will be the interpolated quantity */
		q = NULL;
		if (*s == '$')
			q = Macro[*++s & 0177];

		/*
		**  Interpolate q or output one character
		*/

		if (q != NULL)
			bp = expand(q, bp, buflim);
		else if (bp < buflim - 1)
			*bp++ = *s;
	}
	*bp = '\0';

# ifdef DEBUG
	if (Debug)
		printf("expand ==> '%s'\n", buf);
# endif DEBUG

	return (bp);
}
/*
**  DEFINE -- define a macro.
**
**	this would be better done using a #define macro.
**
**	Parameters:
**		n -- the macro name.
**		v -- the macro value.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Macro[n] is defined.
*/

define(n, v)
	char n;
	char *v;
{
# ifdef DEBUG
	if (Debug)
		printf("define(%c as %s)\n", n, v);
# endif DEBUG
	Macro[n & 0177] = v;
}
