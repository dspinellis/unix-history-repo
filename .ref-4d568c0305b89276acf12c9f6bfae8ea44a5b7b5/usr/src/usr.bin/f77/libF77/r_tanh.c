/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)r_tanh.c	5.3 (Berkeley) %G%";
#endif /* not lint */

float r_tanh(x)
float *x;
{
double tanh();
return( tanh(*x) );
}
