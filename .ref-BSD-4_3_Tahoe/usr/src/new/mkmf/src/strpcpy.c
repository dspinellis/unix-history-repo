/* $Header: strpcpy.c,v 1.1 85/03/14 17:00:27 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * strpcpy() copies string s2 to s1 and returns a pointer to the
 * next character after s1.
 */
char *
strpcpy(s1, s2)
	register char *s1;
	register char *s2;
{
	while (*s1++ = *s2++)
		continue;
	return(--s1);
}
