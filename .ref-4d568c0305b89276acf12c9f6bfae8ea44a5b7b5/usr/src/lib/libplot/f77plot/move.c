/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	8.1 (Berkeley) %G%";
#endif /* not lint */

move_(xi,yi)
int *xi, *yi;
{
	move(*xi,*yi);
}
