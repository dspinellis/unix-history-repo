/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)i_sign.c	5.2 (Berkeley) %G%";
#endif /* not lint */

long int i_sign(a,b)
long int *a, *b;
{
long int x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}
