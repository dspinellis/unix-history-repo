/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)matchhdr.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

# include <stdio.h>
# include <ctype.h>
# include <useful.h>

SCCSID(@(#)matchhdr.c	5.4		6/1/90);

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
