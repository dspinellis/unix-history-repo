/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)z_exp.c	5.1	%G%
 */

#include "complex"

z_exp(r, z)
dcomplex *r, *z;
{
double expx;
double exp(), cos(), sin();

expx = exp(z->dreal);
r->dreal = expx * cos(z->dimag);
r->dimag = expx * sin(z->dimag);
}
