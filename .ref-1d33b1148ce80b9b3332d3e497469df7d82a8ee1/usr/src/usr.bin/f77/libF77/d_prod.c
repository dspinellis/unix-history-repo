/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_prod.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double d_prod(x,y)
float *x, *y;
{
return( (*x) * (*y) );
}
