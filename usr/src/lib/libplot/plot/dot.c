/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)dot.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
dot(xi,yi,dx,n,pat)
int  pat[];
{
	int i;
	putc('d',stdout);
	putsi(xi);
	putsi(yi);
	putsi(dx);
	putsi(n);
	for(i=0; i<n; i++)putsi(pat[i]);
}
