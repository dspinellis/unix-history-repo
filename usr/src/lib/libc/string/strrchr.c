#ifndef lint
static char sccsid[] = "@(#)strrchr.c	5.1 (berkeley) 85/08/04";
#endif not lint

/*
 * Return the ptr in sp at which the character c last
 * appears; NULL if not found
 *
 * This routine is just "rindex" renamed.
 */

#define NULL 0

char *
strrchr(sp, c)
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
