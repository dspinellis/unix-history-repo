/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)dot.c	8.1 (Berkeley) %G%";
#endif /* not lint */

dot_(xi, yi, dx, n, pat)
int *xi, *yi, *dx, *n, *pat;
{
	dot( *xi, *yi, *dx, *n, pat);
}
