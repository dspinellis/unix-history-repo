/*-
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)frame.c	8.1 (Berkeley) %G%";
#endif /* not lint */

frame(n)
{
	extern vti;
	n=n&0377 | 02000;
	write(vti,&n,2);
}
