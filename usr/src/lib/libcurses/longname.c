/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)longname.c	8.1 (Berkeley) %G%";
#endif	/* not lint */

/*
 * longname --
 *	Fill in "def" with the long name of the terminal.
 */
char *
longname(bp, def)
	register char *bp, *def;
{
	register char *cp;

	while (*bp && *bp != ':' && *bp != '|')
		bp++;
	if (*bp == '|') {
		for (cp = def, ++bp; *bp && *bp != ':' && *bp != '|';)
			*cp++ = *bp++;
		*cp = '\0';
	}
	return (def);
}
