#ifndef lint
static char sccsid[] = "@(#)strchr.c	5.1 (Berkeley) 85/08/04";
#endif not lint

/*
 * Return the ptr in sp at which the character c appears;
 * NULL if not found
 *
 * this routine is just "index" renamed.
 */

#define	NULL	0

char *
strchr(sp, c)
register char *sp, c;
{
	do {
		if (*sp == c)
			return(sp);
	} while (*sp++);
	return(NULL);
}
