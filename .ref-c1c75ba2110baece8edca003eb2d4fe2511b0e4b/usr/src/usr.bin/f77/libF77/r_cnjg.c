/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_cnjg.c	5.1	%G%
 */

#include "complex"

r_cnjg(r, z)
complex *r, *z;
{
r->real = z->real;
r->imag = - z->imag;
}
