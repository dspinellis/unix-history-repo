/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)genget.c	8.1 (Berkeley) %G%";
#endif /* not lint */


#include <ctype.h>

#define	LOWER(x) (isupper(x) ? tolower(x) : (x))
/*
 * The prefix function returns 0 if *s1 is not a prefix
 * of *s2.  If *s1 exactly matches *s2, the negative of
 * the length is returned.  If *s1 is a prefix of *s2,
 * the length of *s1 is returned.
 */
	int
isprefix(s1, s2)
	register char *s1, *s2;
{
        register int n = 0;
        char *os1;
	register char c1, c2;

        if (*s1 == '\0')
                return(-1);
        os1 = s1;
	c1 = *s1;
	c2 = *s2;
        while (LOWER(c1) == LOWER(c2)) {
		if (c1 == '\0')
			break;
                c1 = *++s1;
                c2 = *++s2;
        }
        return(*s1 ? 0 : (*s2 ? (s1 - os1) : (os1 - s1)));
}

static char *ambiguous;		/* special return value for command routines */

	char **
genget(name, table, stlen)
	char	*name;		/* name to match */
	char	**table;	/* name entry in table */
	int	stlen;
{
	register char **c, **found;
	register int n;

	if (name == 0)
	    return 0;

	found = 0;
	for (c = table; *c != 0; c = (char **)((char *)c + stlen)) {
		if ((n = isprefix(name, *c)) == 0)
			continue;
		if (n < 0)		/* exact match */
			return(c);
		if (found)
			return(&ambiguous);
		found = c;
	}
	return(found);
}

/*
 * Function call version of Ambiguous()
 */
	int
Ambiguous(s)
	char *s;
{
	return((char **)s == &ambiguous);
}
