/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)c_abs.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "complex"

float c_abs(z)
complex *z;
{
double cabs();

return( cabs( z->real, z->imag ) );
}
