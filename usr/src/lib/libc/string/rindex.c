#ifndef lint
static char sccsid[] = "@(#)rindex.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Return the ptr in sp at which the character c last
 * appears; NULL if not found
 */

#define NULL 0

char *
rindex(sp, c)
register char *sp, c;
{
	register char *r;

	r = NULL;
	do {
		if (*sp == c)
			r = sp;
	} while (*sp++);
	return(r);
}
