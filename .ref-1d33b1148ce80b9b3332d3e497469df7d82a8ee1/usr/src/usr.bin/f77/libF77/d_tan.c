/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_tan.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double d_tan(x)
double *x;
{
double tan();
return( tan(*x) );
}
