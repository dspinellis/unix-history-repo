# define	reg	register

/*
 *	This routine fills in "def" with the full name of the terminal.
 * This is assumed to be the last name in the list of aliases.
 *
 * @(#)fullname.c	1.1 (Berkeley) %G%
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
