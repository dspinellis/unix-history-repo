/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)DATE.c	1.2 (Berkeley) %G%";
#endif /* not lint */

char	_pd_date[] = {
	8, 9, 10, 4, 5, 6, 10, 22, 23, 10, 0
};

extern char *ctime();

DATE(alfap)

	register char *alfap;
{
	register char *ap, *cp, *dp;
	long a;

	time(&a);
	cp = ctime(&a);
	ap = alfap;
	for (dp = _pd_date; *dp; *ap++ = cp[*dp++]);
}
