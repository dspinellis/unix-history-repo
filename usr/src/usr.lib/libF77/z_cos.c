/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)z_cos.c	5.1	6/7/85
 */

#include "complex"

z_cos(r, z)
dcomplex *r, *z;
{
double sin(), cos(), sinh(), cosh();

r->dreal = cos(z->dreal) * cosh(z->dimag);
r->dimag = - sin(z->dreal) * sinh(z->dimag);
}
