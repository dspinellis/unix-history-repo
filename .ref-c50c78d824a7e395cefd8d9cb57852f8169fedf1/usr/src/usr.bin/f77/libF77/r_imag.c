/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_imag.c	5.2	%G%
 */

#include "complex"

float r_imag(z)
complex *z;
{
return(z->imag);
}
