/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)fullname.c	8.1 (Berkeley) %G%";
#endif	/* not lint */

/*
 * fullname --
 *	This routine fills in "def" with the full name of the terminal.
 *	This is assumed to be the last name in the list of aliases.
 */
char *
fullname(bp, def)
	register char *bp, *def;
{
	register char *cp;

	*def = '\0';		/* In case no name. */

	while (*bp && *bp != ':') {
		cp = def;	/* Start of answer. */
		while (*bp && *bp != ':' && *bp != '|')
			*cp++ = *bp++;	/* Copy name over. */
		*cp = '\0';		/* Zero end of name. */
		if (*bp == '|')
			bp++;		/* Skip over '|' if that is case. */
	}
	return (def);
}
