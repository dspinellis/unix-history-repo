/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_tanh.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double d_tanh(x)
double *x;
{
double tanh();
return( tanh(*x) );
}
