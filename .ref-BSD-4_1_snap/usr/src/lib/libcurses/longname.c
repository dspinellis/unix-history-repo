# define	reg	register

/*
 *	This routine returns the long name of the terminal or "def"
 *	if none can be found.
 *
 * 5/15/81 (Berkeley) @(#)longname.c	1.2
 */
char *
longname(bp, def)
reg char	*bp, *def; {

	reg char	*cp;
	static char ttytype[20];
	char save;

	while (*bp && *bp != ':' && *bp != '|')
		bp++;
	if (*bp == '|') {
		bp++;
		cp = bp;
		while (*cp && *cp != ':' && *cp != '|')
			cp++;
		save = *cp;
		*cp = 0;
		strcpy(ttytype, bp);
		*cp = save;
		return ttytype;
	}
	strcpy(ttytype, def);
	return ttytype;
}
