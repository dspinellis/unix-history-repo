/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)c_abs.c	5.1	6/7/85
 */

#include "complex"

float c_abs(z)
complex *z;
{
double cabs();

return( cabs( z->real, z->imag ) );
}
