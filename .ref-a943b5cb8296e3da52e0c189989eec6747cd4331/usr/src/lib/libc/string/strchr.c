#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strchr.c	5.2 (Berkeley) 86/03/09";
#endif LIBC_SCCS and not lint

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
