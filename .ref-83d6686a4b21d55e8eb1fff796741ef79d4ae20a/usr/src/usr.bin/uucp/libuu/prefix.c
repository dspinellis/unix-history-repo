#ifndef lint
static char sccsid[] = "@(#)prefix.c	5.3 (Berkeley) %G%";
#endif

/*LINTLIBRARY*/

/*
 *	check s2 for prefix s1
 *
 *	return 0 - !=
 *	return 1 - == 
 */

prefix(s1, s2)
register char *s1, *s2;
{
	register char c;

	while ((c = *s1++) == *s2++)
		if (c == '\0')
			return 1;
	return c == '\0';
}

/*
 *	check s2 for prefix s1 with a wildcard character ?
 *
 *	return 0 - !=
 *	return 1 - == 
 */

wprefix(s1, s2)
register char *s1, *s2;
{
	register char c;

	while ((c = *s1++) != '\0')
		if (*s2 == '\0'  ||  (c != *s2++  &&  c != '?'))
			return 0;
	return 1;
}
