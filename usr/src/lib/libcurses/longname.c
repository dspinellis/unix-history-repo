# define	reg	register

/*
 *	This routine fills in "def" with the long name of the terminal.
 *
 * @(#)longname.c	1.2 (Berkeley) %G%
 */
char *
longname(bp, def)
reg char	*bp, *def; {

	reg char	*cp;

	while (*bp && *bp != ':' && *bp != '|')
		bp++;
	if (*bp == '|') {
		bp++;
		cp = def;
		while (*bp && *bp != ':' && *bp != '|')
			*cp++ = *bp++;
		*cp = 0;
	}
	return def;
}
