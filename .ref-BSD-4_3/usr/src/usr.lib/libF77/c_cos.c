/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)c_cos.c	5.1	6/7/85
 */

#include "complex"

c_cos(r, z)
complex *r, *z;
{
double sin(), cos(), sinh(), cosh();

r->real = cos(z->real) * cosh(z->imag);
r->imag = - sin(z->real) * sinh(z->imag);
}
