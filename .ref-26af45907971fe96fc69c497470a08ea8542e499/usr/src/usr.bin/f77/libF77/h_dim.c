/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)h_dim.c	5.2 (Berkeley) %G%";
#endif /* not lint */

short h_dim(a,b)
short *a, *b;
{
return( *a > *b ? *a - *b : 0);
}
