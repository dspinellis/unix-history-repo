/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
*/

#ifndef lint
static char	SccsId[] = "@(#)matchhdr.c	5.1 (Berkeley) 6/7/85";
#endif not lint

# include <stdio.h>
# include <ctype.h>
# include "useful.h"

SCCSID(@(#)matchhdr.c	5.1		6/7/85);

/*
**  MATCHHDR -- Match header line
**
**	Matches a header line in arpanet format (case and white
**	space is ignored).
**
**	This routine is used by arpa-mailer and sendmail.
**
**	Parameters:
**		line -- the line to match against.
**		pat -- the pattern to match against; must be in
**			lower case.
**
**	Returns:
**		address of the 'value' of the pattern (the beginning
**			of the non-white string following the delim).
**		NULL if none found.
**
**	Side Effects:
**		none
**
**	Called By:
**		maketemp
**		sendmail [arpa.c]
**
**	Deficiencies:
**		It doesn't handle folded lines.
*/

char *
matchhdr(line, pat)
	char *line;
	char *pat;
{
	register char *p;
	register char *q;

	for (q = pat, p = line; *q != '\0'; p++, q++)
		if (lowercase(*p) != *q)
			return (NULL);
	while (isspace(*p))
		p++;
	if (*p != ':')
		return (NULL);
	while (isspace(*++p))
		continue;
	return (*p == '\0' ? NULL : p);
}
/*
**  LOWERCASE -- Convert a character to lower case
**
**	If the argument is an upper case letter, it is converted
**	to a lower case letter, otherwise it is passed through
**	unchanged.
**
**	Parameters:
**		c -- the character to check.
**
**	Returns:
**		c converted to lower case.
**
**	Side Effects:
**		none
**
**	Called By:
**		matchhdr
*/

lowercase(c)
	register char c;
{
	if (isupper(c))
		c -= 'A' - 'a';
	return (c);
}
