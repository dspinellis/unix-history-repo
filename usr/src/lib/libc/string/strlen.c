#ifndef lint
static char sccsid[] = "@(#)strlen.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Returns the number of
 * non-NULL bytes in string argument.
 */

strlen(s)
register char *s;
{
	register n;

	n = 0;
	while (*s++)
		n++;
	return(n);
}
