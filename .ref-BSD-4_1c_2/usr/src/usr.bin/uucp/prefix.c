static char sccsid[] = "@(#)prefix.c	4.1	(Berkeley)	9/11/82";



/*******
 *	prefix(s1, s2)	check s2 for prefix s1
 *	char *s1, *s2;
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
			return(1);
	return(c == '\0');
}

/*******
 *	wprefix(s1, s2)	check s2 for prefix s1 with a wildcard character ?
 *	char *s1, *s2;
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
			return(0);
	return(1);
}
