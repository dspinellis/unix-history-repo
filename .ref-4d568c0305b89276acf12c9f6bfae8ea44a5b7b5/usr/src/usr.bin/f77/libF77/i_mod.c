/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)i_mod.c	5.2 (Berkeley) %G%";
#endif /* not lint */

long int i_mod(a,b)
long int *a, *b;
{
return( *a % *b);
}
