/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)c_exp.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "complex"

c_exp(r, z)
complex *r, *z;
{
double expx;
double exp(), cos(), sin();

expx = exp(z->real);
r->real = expx * cos(z->imag);
r->imag = expx * sin(z->imag);
}
