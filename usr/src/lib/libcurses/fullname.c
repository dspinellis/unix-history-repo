/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)fullname.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# define	reg	register

/*
 *	This routine fills in "def" with the full name of the terminal.
 * This is assumed to be the last name in the list of aliases.
 *
 */
char *
fullname(bp, def)
reg char	*bp, *def;
{

	reg char	*cp;

	*def = 0;			/* in case no name */

	while (*bp && *bp != ':') {
		cp = def;		/* start of answer */
		while (*bp && *bp != ':' && *bp != '|') {
			*cp++ = *bp++;	/* copy name over */
		}
		*cp = 0;		/* zero end of name */
		if (*bp == '|') {
			bp++;		/* skip over '|' if that is case */
		}
	}
	return(def);
}
