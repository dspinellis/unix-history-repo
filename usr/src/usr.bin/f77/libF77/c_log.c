/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)c_log.c	5.1	%G%
 */

#include "complex"

c_log(r, z)
complex *r, *z;
{
double log(), cabs(), atan2();

r->imag = atan2(z->imag, z->real);
r->real = log( cabs(z->real, z->imag) );
}
